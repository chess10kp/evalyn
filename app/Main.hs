{-# LANGUAGE ExistentialQuantification #-}
-- to create heterogeneous typeclasses
module Main where

import System.IO
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Control.Monad.Except
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError -- partial type

type IOThrowsError = ExceptT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Float Float
             | Character Char
             | Bool Bool
             | Func { params :: [String], vararg :: Maybe String,
                    body :: [LispVal], closure :: Env }
             | PrimitiveFunc  ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
                 | TypeMismatch String LispVal
                 | Parser ParseError
                 | BadSpecialForm String LispVal
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseChar :: Parser LispVal
parseChar = do
  _ <- char '\''
  x <- letter
  _ <- char '\''
  return $ Character x

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many ( tab <|> newline <|> noneOf "\"") -- TODO: implement escaping
    _ <- char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber needs to check for both Float and Int
parseNumber = do many1 digit >>= \x -> return (Number . read $ x)

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  return $ Float $ read $ x ++ "." ++ y

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t


parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [nonList] = throwError $ TypeMismatch "pair" nonList
car nonListArg = throwError $ NumArgs 1 nonListArg

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]           = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [nonList]               = throwError $ TypeMismatch "pair" nonList
cdr nonList                 = throwError $ NumArgs 1 nonList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [List [] , List []]      = return $ List []
cons [List (x:[]), List xs]   = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast -- dottedLists remain DottedLists
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgs                  = throwError $ NumArgs 2 badArgs

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             all eqvPair (zip arg1 arg2)
                                                             where eqvPair (x1, x2) = case eqv [x1,x2] of
                                                                     Left err -> False
                                                                     Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do _ <- char '('
                   x <- try parseList <|> parseDottedList
                   _ <- char ')'
                   return x

-- display values
showVal :: LispVal -> String
showVal (String contents)    = "\"" ++ contents  ++ "\""
showVal (Character contents) = "'" ++ show contents ++ "'"
showVal (Atom name)          = name
showVal (Number contents)    = show contents
showVal (Bool True)          = "#t"
showVal (Bool False)         = "#f"
showVal (Float contents)     = show contents
showVal (List contents)      = "(" ++ unwordsList  contents ++ ")"
showVal (DottedList h t)     = "(" ++ unwordsList  h ++ "." ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = "(lambda (" ++ unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO Port>"
showVal (IOFunc _) = "<IO Primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args

apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env

apply (IOFunc func) args = func args

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
     where makeFunc constructor (var, func) = (var, constructor func)


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool


boolBinop :: (LispVal -> ThrowsError a) -> (a->a->Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right



primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("||", boolBoolBinop (||)),
              ("&&", boolBoolBinop (&&)),
              ("string=?", numBoolBinop (==)),
              ("string<?", numBoolBinop (<)),
              ("string>?", numBoolBinop (>)),
              ("string>=?", numBoolBinop (>=)),
              ("string<=?", numBoolBinop (<=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("sortiz", qsortiz)]


qsortiz :: [LispVal] -> ThrowsError LispVal
qsortiz [List []]           = return $ List []
qsortiz [List [x]]          = return $ List [x]
qsortiz [List (xs)]           = do
  let numbers = mapM unpackNum xs
  case numbers of
    Right nums -> return $ List $ map Number (qsort nums)
    Left err -> throwError err
  where
    qsort :: [Integer] -> [Integer]
    qsort [] = []
    qsort (y:ys) = qsort [a | a <- ys, a <= y] ++ [y] ++ qsort [a | a <- ys, a > y]
qsortiz [badArg] = throwError $ TypeMismatch "list of numbers" badArg
qsortiz badArg                   = throwError $ TypeMismatch "list" $ String "bad"

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
  if null parsed
  then throwError $ TypeMismatch "number" $ String n
  else
    return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notString = throwError $ TypeMismatch "boolean" notString

-- eval :: LispVal -> ThrowsError LispVal
-- eval val@(String _) = return val
-- eval val@(Number _) = return val
-- eval val@(Bool _) = return val
-- eval (List [Atom "quote", val]) = return val
-- eval (List [Atom "if", pred, conseq, alt]) =
--   do result <- eval pred
--      case result of
--        Bool False -> eval alt 
--        _ -> eval conseq
-- eval (List (Atom func: args)) = mapM eval args >>= apply func
-- eval badForm = throwError $ BadSpecialForm "Unrecognized form" badForm

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var


eval env (List [Atom "load", String filename]) =  load filename >>= liftM last . mapM (eval env)
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body


eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


instance Show LispVal where show = showVal

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func ++ " not found"
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpack1 <- unpacker arg1
     unpack2 <- unpacker arg2
     return $ unpack1 == unpack2
   `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArg = throwError $ NumArgs 2 badArg

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "This should not happen"


readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)


ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


-- evalString :: String -> IO String
-- evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

-- evalAndPrint :: String -> IO ()
-- evalAndPrint expr = evalString expr >>= putStrLn

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env



until_ :: Monad  m => (a->Bool) -> m a -> (a-> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool -- check if a var is already bound
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)


setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value


defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

-- runRepl :: IO ()
-- runRepl = until_ (== "quit") (readPrompt "Evalyn>>> ") evalAndPrint


runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "evalyn>>> ") . evalAndPrint


makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

nullEnv :: IO Env
nullEnv = newIORef []

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

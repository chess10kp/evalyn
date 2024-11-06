module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Control.Monad.Except

type ThrowsError = Either LispError -- partial type

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Float Float
             | Character Char
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
                 | TypeMismatch String LispVal
                 | Parser ParseError
                 | BadSpecialForm String LispVal
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String


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
parseNumber = do
        many1 digit >>= \x -> return (Number . read $ x)

-- parsePrefixedNumber :: Parser LispVal
-- parsePrefixedNumber = do
--             _ <- oneOf "#"
--             base <- oneOf "bodx"
--             digits <- many1 digit
--             return $ Number $ val base digits
--                 where
--                 val :: Char -> String -> Integer
--                 val base digits 
--                         | Just (x,_) <- maybeOutput = x
--                         | otherwise = error "Invalid number format"
--                         where
--                             maybeOutput = 
--                                 case base of  
--                                     'o' ->  listToMaybe $ readOct digits
--                                     'x' ->  listToMaybe $ readHex digits
--                                     'b' ->  listToMaybe $ readBin digits
--                                     _ ->  read digits


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
car nonListArg = car [nonList] = throwError $ NumArgs 1 nonListArg

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [nonList] = throwError $ TypeMismatch "pair" nonList
cdr nonList = throwError $ NumArgs 1 nonList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [List [] , List []] = return $ List []
cons [List (x:[]), List xs] = return $ List x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast -- dottedLists remain DottedLists
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgs = throwError $ NumArgs 2 badArgs

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
showVal (String contents) = "\"" ++ contents  ++ "\""
showVal (Character contents) = "'" ++ show contents ++ "'"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float contents) = show contents

showVal (List contents) = "(" ++ unwordsList  contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList  h ++ "." ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive func args" func )
                  ($ args)
                  (lookup func primitives)


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
              ("string<=?", numBoolBinop (<=))
             ]

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n   
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
  if null parsed
  then throwError $ TypeMismatch "number" $ String n
  else
    return $ fst $ parsed !! 0 
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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  do result <- eval pred
     case result of
       Bool False -> eval alt 
       _ -> eval conseq
eval (List (Atom func: args)) = mapM eval args >>= apply func
       
eval badForm = throwError $ BadSpecialForm "Unrecognized form" badForm

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


extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "This should not happen"
  
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err 
  Right val -> return val


main  :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

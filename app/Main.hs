module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Control.Monad.Except

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
    x <- many ( oneOf "\\\"" <|> noneOf "\"")
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
  
parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do _ <- char '('
                   x <- try parseList <|> parseDottedList
                   _ <- char ')'
                   return x


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

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

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args ) $ lookup func primitives


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]
             
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n   
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
  if null parsed
  then 0
  else
    fst $ head parsed 
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func: args)) = apply func $ map eval args


instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
                 | TypeMismatch String LispVal
                 | Parser ParseError
                 | BadSpecialForm String LispVal
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func ++ " not found"
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError -- partial type

data LispVal =
  Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Float Float
  | Character Char
  | Bool Bool
  
main  :: IO ()
main = getArgs >>= print . eval . readExpr . head

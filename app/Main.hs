module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Prelude hiding (head) 
import System.Environment (getArgs)
import Numeric (readHex, readOct, readBin)
import Data.Maybe (listToMaybe )
import Control.Monad (liftM)

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
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
  
parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"


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
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

  

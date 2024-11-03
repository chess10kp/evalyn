module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ String x

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol ) "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"


data LispVal =
  Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  
main  :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)


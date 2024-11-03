module Main where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)

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
  -- parseNumber = liftM (Number . read) $ many1 digit
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
parseNumber = do
  many1 digit >>= \x -> return (Number . read  $  x)

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

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


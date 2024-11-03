module Main where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Numeric (readFloat, readHex, readOct, readBin)
import Data.Maybe (listToMaybe, isNothing, fromJust)

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
parseNumber = do
  _ <- oneOf "#"
  base <- oneOf "bodx"
  digits <- many1 digit
  return $ Number (val base digits)
    where
      val :: Char -> String -> Integer
      val base digits = output
        where
          output :: Integer
          output
                | Just (x,_) <- maybeOutput = x
                | otherwise = error "Invalid number format"
                where
                  maybeOutput :: Maybe (Integer, String)
                  maybeOutput = 
                        case base of  
                            'o' ->  listToMaybe $ readOct digits
                            _ ->  read digits

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

  

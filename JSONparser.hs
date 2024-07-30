{-# LANGUAGE LambdaCase #-}

module Haskell.JSONparser where

import Control.Applicative (Alternative (empty), many, (<|>))
import Data.Char (isDigit, isSpace, ord)
import Distribution.Utils.Json (Json)

-- JSON constructor for the values struct
-- comments ?
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNum Int -- float support ?
  | JsonStr String
  | JsonArr [JsonValue]
  | JsonObj [(String, JsonValue)]
  deriving (Show, Eq)

-- Define the parser type,
-- error checking ?
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

-- Proofs
-- Prove that Parser is Functor
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

-- Proce that Parser is Applicative
instance Applicative Parser where
  pure p = Parser $ \input -> Just (input, p)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

-- Prove that Parser is Alternative
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

-- Aux functions for the parsers
-- Value
charP :: Char -> Parser Char
charP x = Parser $ \case
  y : ys | x == y -> Just (ys, x)
  _ -> Nothing

-- Value
stringP :: String -> Parser String
stringP = traverse charP

-- Num, Arr
spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

-- Num
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs then Nothing else Just (input', xs)

-- Str, Obj
-- escaping ?
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

-- Arr, Obj
ws :: Parser String
ws = spanP isSpace

-- Arr, Obj
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep elm = (:) <$> elm <*> many (sep *> elm) <|> pure []

-- Type specific parsers
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    _ = Nothing

jsonNum :: Parser JsonValue
jsonNum = f <$> notNull (spanP isDigit)
  where
    f ds = JsonNum $ read ds

jsonStr :: Parser JsonValue
jsonStr = JsonStr <$> stringLiteral

jsonArr :: Parser JsonValue
jsonArr = JsonArr <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy sep jsonValue
    sep = ws *> charP ',' <* ws

jsonObj :: Parser JsonValue
jsonObj = JsonObj <$> (charP '{' *> ws *> pairs <* ws <* charP '}')
  where
    pairs = sepBy (ws *> charP ',' <* ws) pair
    pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' <* ws) <*> jsonValue

-- Combined final parser
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNum <|> jsonStr <|> jsonArr <|> jsonObj

-- Hepler to parse files
parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = undefined
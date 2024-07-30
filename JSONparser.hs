{-# LANGUAGE LambdaCase #-}

module Haskell.JSONparser where

import Control.Applicative (Alternative (empty), many, some, (<|>))
import Data.Char (isDigit, isSpace, ord)
import Data.Text.Internal.Builder.Int.Digits (digits)

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

-- Parsing error support
data ParseError = ParseError Int String deriving (Show)

-- Define the parser type
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

-- Num
-- Signature meaning: Sign, Whole part, Decimal part, exponent -> Double
doubleParts :: Int -> Int -> Double -> Int -> Double
doubleParts sign int dec exp = fromIntegral sign * (fromIntegral int + dec) * (10 ^^ exp)

-- Num
doubleLiteral :: Parser Double
doubleLiteral = undefined

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
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

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

-- Keeps only the produced JSON
jsonParse :: String -> Maybe JsonValue
jsonParse input = snd <$> runParser jsonValue input

-- Hepler to parse files
parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = undefined
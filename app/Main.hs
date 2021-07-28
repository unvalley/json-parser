{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Main where

import Data.Char
import Control.Applicative ( Alternative((<|>), empty) )

-- JSON型定義
-- https://scrapbox.io/kirohi-tech/data%E3%81%A8newtype
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  -- Key: String, Value: JsonValue
  | JsonObject [(String, JsonValue)]
  -- deriving: JsonValueが ShowとEqのinstanceであることを宣言
  deriving (Show, Eq)

-- NOTE: no propser error reporting, for composable
-- 型シノニム
-- newtypeを使う場合は，必ずコンストラクタが一つ，フィールドが一つとなる
-- newtypeを使うことで，効率を犠牲にすることなく安全性が高められる
newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}


-- ⇓ 何やってるの？
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = 
    Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where -- ローカル変数定義など
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing


stringP :: String -> Parser String
stringP input = traverse charP
-- stringP = sequenceA . map charP

jsonBool :: Parser JsonValue
jsonBool = f <$> stringP "true" <|> stringP "false"
  where f "true"  = JsonBool True
        f "false" = JsonBool False
        -- This shuld never happen
        f _       = undefined

-- letはdoがない場合，最後にinが必要
spanP :: (Char -> Bool) -> Parser String
spanP f = 
  Parser $ \input -> let (token, rest) span f input in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
            (input', xs) <- runParser p input
            if null xs
              then Nothing
              else (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where f ds = JsonNumber $ read ds

-- NOTE: no escape support
stringLiteral :: Parser String
stringLiteral spanP (/= '"')

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"" *> stringLiteral <* charP '"')

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonstring


main :: IO ()
main = undefined

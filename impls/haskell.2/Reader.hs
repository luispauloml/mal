module Reader where

import Control.Applicative
import Control.Monad
import Data.Char

import Types

-- ------------------------------------------------------------
-- Parser type

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (v, input') <- p input
      Just (f v, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser f) <*> (Parser a) =
    Parser $ \input -> do
      (v, input') <- f input
      (u, input'') <- a input'
      return (v u, input'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p) <|> (Parser q) =
    Parser $ \input -> p input <|> q input

instance Monad Parser where
  (Parser p) >>= f =
    Parser $ \str -> p str >>=
             \(a, rst) -> runParser (f a) rst

-- ------------------------------------------------------------
-- Primitve operations

nextP :: Parser Char
nextP = Parser $ \str -> case str of
  (c:s) -> return (c, s)
  _     -> Nothing

charP :: Char -> Parser Char
charP c = do
  x <- nextP
  guard (x == c)
  return c

stringP :: String -> Parser String
stringP s = sequence $ map charP s

-- ------------------------------------------------------------
-- Parsers

nilP :: Parser LispVal
nilP = const Nil <$> stringP "()"

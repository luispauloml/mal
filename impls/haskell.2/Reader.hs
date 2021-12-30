module Reader where

import Control.Applicative

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

module Reader where

import Control.Applicative
import Control.Monad
import Data.Char

import Text.Read.Lex (readDecP)
import Text.ParserCombinators.ReadP (readP_to_S)

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
  (c:s) -> Just (c, s)
  _     -> Nothing

charP :: Char -> Parser Char
charP c = checkNextP (== c)

lookP :: Parser String
lookP = Parser $ \str -> Just (str, str)

takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP p = Parser $ \str ->
  Just (takeWhile p str, dropWhile p str)

checkNextP :: (Char -> Bool) -> Parser Char
checkNextP p = do
  c <- nextP
  guard (p c)
  return c

checkAndReparse :: Parser check -> Parser a -> Parser a
checkAndReparse check p = Parser $ \str -> runParser check str >> runParser p str

stringP :: String -> Parser String
stringP s = sequence $ map charP s

enclosedP :: Parser open -> Parser close -> Parser a -> Parser a
enclosedP open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

enclosedOptionalP :: Parser open -> Parser close -> Parser a -> Parser a
enclosedOptionalP open close p  = before <|> between <|> after
  where before  = do open; p
        between = enclosedP open close p
        after   = do x <- p; close; return x

whitespaceP :: Parser String
whitespaceP = takeWhileP isSpace

-- ------------------------------------------------------------
-- Auxiliary functions

isPunctOrSpace :: Char -> Bool
isPunctOrSpace c = or $ map ($c) [isPunctuation, isSpace]

-- ------------------------------------------------------------
-- Parsers

lispNilP :: Parser LispVal
lispNilP = const Nil <$> stringP "()"

lispTrueP :: Parser LispVal
lispTrueP = const LispTrue <$> do
  t <- charP 't'
  rst <- lookP
  let check = runParser (checkNextP isPunctOrSpace) rst
  guard $ null rst || maybe False (const True) check
  return t

lispIntP :: Parser LispVal
lispIntP = fmap Int $ Parser $ \str -> convert $ readP_to_S readDecP str
  where convert [] = Nothing
        convert [(n, c)] = case c of
          [] -> Just (n, c)
          _  -> runParser (checkNextP isPunctOrSpace) c >> Just (n, c)

lispStringP :: Parser LispVal
lispStringP = String <$> enclosedP (charP '"') (charP '"') (takeWhileP (/= '"'))

lispEnclosedP :: ([LispVal] -> LispVal)
              -> Parser open
              -> Parser close
              -> Parser LispVal
lispEnclosedP conv op cl
  = conv <$> enclosedP open close (many $ whitespaceP >> lispValP)
  where enclosedBySpace p = enclosedOptionalP whitespaceP whitespaceP p
        open              = enclosedBySpace $ op
        close             = enclosedBySpace $ cl

lispListP :: Parser LispVal
lispListP = lispEnclosedP listToNil (charP '(') (charP ')')
  where listToNil [] = Nil
        listToNil a  = List a

lispVectP :: Parser LispVal
lispVectP = lispEnclosedP Vector (charP '[') (charP ']')

lispSetP :: Parser LispVal
lispSetP = lispEnclosedP Set (charP '{') (charP '}')

lispAtomP :: Parser LispVal
lispAtomP = Atom <$> checkAndReparse n y
  where n = checkNextP (\c -> not $ or $ map ($c) notCases)
        y = takeWhileP (\c -> and $ map ($ c) cases)
        notCases = [isNumber]          ++ map (==) "'\"()[]{}"
        cases    = [not . isSeparator] ++ map (/=) "()[]{}"

lispQuoteP :: Parser LispVal
lispQuoteP = Quote <$> checkAndReparse (charP '\'') (nextP >> lispValP)

lispValP :: Parser LispVal
lispValP =  lispNilP    <|> lispIntP  <|> lispTrueP
        <|> lispStringP <|> lispAtomP <|> lispListP
        <|> lispQuoteP  <|> lispVectP <|> lispSetP

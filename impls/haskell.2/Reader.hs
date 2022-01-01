module Reader where

import Control.Applicative
import Control.Monad
import Data.Char

import Text.Read (readMaybe)
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

failP :: Parser a
failP = Parser $ const Nothing

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

choiceP :: [Parser a] -> Parser a
choiceP ls = foldr (<|>) failP ls

-- ------------------------------------------------------------
-- Auxiliary functions

isPunctOrSpace :: Char -> Bool
isPunctOrSpace c = or $ map ($c) [isPunctuation, isSpace]

headMaybe :: [a] -> Maybe a
headMaybe []     = Nothing
headMaybe (x:xs) = Just x

readP_to_Parser p = Parser $ \str -> headMaybe $ readP_to_S p str

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
lispIntP  = fmap Int $ positive <|> (negate <$> negative)
  where positive = readP_to_Parser readDecP
        negative = charP '-' >> positive

lispStringP :: Parser LispVal
lispStringP = let read' = readMaybe :: String -> Maybe String
              in fmap String $ Parser $ \str -> do
                 (a, b) <- headMaybe (lex str)
                 s      <- read' a
                 return (s, b)

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
        notCases = [isNumber, isPunctuation] ++ map (==) "'`~\"()[]{}@"
        cases    = [not . isSeparator] ++ map (/=) "()[]{}"

lispKwP :: Parser LispVal
lispKwP = let atomMap f (Atom s) = Atom (f s) in
          do charP ':'
             a <- lispAtomP
             return $ atomMap (':':) a

lispOpP :: Parser LispVal
lispOpP = fmap Atom $ singleChar <|> multiChar
  where multiChar  = choiceP $ map stringP ["->>", "**"]
        singleChar = do
          c <- choiceP $ map charP "-+\\*"
          rst <- lookP
          guard (null rst || isSpace (head rst))
          return [c]

lispQuoteP :: Parser LispVal
lispQuoteP = Quote <$> checkAndReparse (charP '\'') (nextP >> lispValP)

lispQuasiP :: Parser LispVal
lispQuasiP = QuasiQuote <$> checkAndReparse (charP '`') (nextP >> lispValP)

lispUnqtP :: Parser LispVal
lispUnqtP = Unquote <$> checkAndReparse (charP '~') (nextP >> lispValP)

lispSpliceP :: Parser LispVal
lispSpliceP = SpliceUnquote <$>
  checkAndReparse (stringP "~@") (nextP >> nextP >> lispValP)

lispDerefP :: Parser LispVal
lispDerefP = Deref <$>
  checkAndReparse (charP '@') (nextP >> lispValP)

lispValP :: Parser LispVal
lispValP = choiceP [ lispNilP,    lispIntP,   lispTrueP
                   , lispStringP, lispAtomP,  lispListP
                   , lispQuoteP , lispVectP,  lispSetP
                   , lispSpliceP, lispQuasiP, lispUnqtP
                   , lispDerefP , lispKwP,    lispOpP ]

read_str :: String -> Maybe LispVal
read_str str = fst <$> runParser (whitespaceP >> lispValP) str

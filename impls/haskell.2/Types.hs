module Types where

data LispVal
  = Atom String
  | Deref         LispVal
  | Int           Int
  | Keyword       String LispVal
  | LispTrue
  | List          [LispVal]
  | Meta          (LispVal, LispVal)
  | Nil
  | QuasiQuote    LispVal
  | Set           [LispVal]
  | SpliceUnquote LispVal
  | String        String
  | Unquote       LispVal
  | Vector        [LispVal]
  deriving (Show, Eq)

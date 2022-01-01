module Types where

data LispVal
  = Atom String
  | Deref         LispVal
  | Int           Int
  | Keyword       String LispVal
  | LispTrue
  | List          [LispVal]
  | Nil
  | Set           [LispVal]
  | String        String
  | Vector        [LispVal]
  deriving (Show, Eq)

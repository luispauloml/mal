module Types where

data LispVal
  = Atom String
  | Deref         LispVal
  | Int           Int
  | LispTrue
  | List          [LispVal]
  | Nil
  | QuasiQuote    LispVal
  | Quote         LispVal
  | Set           [LispVal]
  | SpliceUnquote LispVal
  | String        String
  | Unquote       LispVal
  | Vector        [LispVal]
  deriving (Show, Eq)

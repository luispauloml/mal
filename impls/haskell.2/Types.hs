module Types where

data LispVal
  = Atom String
  | List [LispVal]
  | Set [LispVal]
  | Vector [LispVal]
  | Int Int
  | String String
  | Nil
  | LispTrue
  | Quote LispVal
  deriving (Show, Eq)

module Types where

data LispVal
  = Symbol        String
  | Map           [(LispVal, LispVal)]
  | Int           Integer
  | Keyword       String LispVal
  | LispTrue
  | List          [LispVal]
  | Nil
  | String        String
  | Vector        [LispVal]
  deriving (Show, Eq)

fromListToMap :: LispVal -> Maybe LispVal
fromListToMap (List ls)
  | length ls `mod` 2 /= 0 = Nothing
  | otherwise =  Just . Map $ mkTuples ls
  where mkTuples [] = []
        mkTuples (k:v:xs) = (k, v) : mkTuples xs
fromListToMap _ = Nothing

toListFromMap :: LispVal -> Maybe LispVal
toListFromMap (Map m) = Just . List $ fromTuples m
  where fromTuples [] = []
        fromTuples ((k, v):xs) = k : v : fromTuples xs
toListFromMap _ = Nothing

fromCollection :: LispVal -> Maybe [LispVal]
fromCollection (List l)   = Just l
fromCollection (Vector v) = Just v
fromCollection m@(Map _)  = toListFromMap m >>= fromCollection
fromCollection _          = Nothing

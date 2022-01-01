module Printer where

import Types

pr_str :: LispVal -> String
pr_str (Atom s)          = s
pr_str (Int i)           = show i
pr_str (String s)        = '\"' : pr_string s ++ "\""
pr_str Nil               = "()"
pr_str LispTrue          = "t"
pr_str (List l)          = pr_enclosed "(" ")" " " l
pr_str (Vector v)        = pr_enclosed "[" "]" " " v
pr_str (Set s)           = pr_enclosed "{" "}" " " s
pr_str (Deref v)         = "(deref "          ++ pr_str v ++ ")"
pr_str (Keyword s v)     = pr_kwd s v

pr_enclosed :: String -> String -> String -> [LispVal] -> String
pr_enclosed o c s l = o ++ worker l
  where worker [] = c
        worker (a:b:vs) = pr_str a ++ s ++ worker (b:vs)
        worker (b:[]) = pr_str b ++ c

pr_kwd :: String -> LispVal -> String
pr_kwd "with-meta" (List (s:v:[])) =
  "(with-meta " ++ pr_str v ++ " " ++ pr_str s ++ ")"
pr_kwd s v =
  "(" ++ s ++ " " ++ pr_str v ++ ")"

pr_string :: String -> String
pr_string = foldr1 (.) replaces
  where replaces = zipWith replace
                           (map (==) $
                            '"'   : '\n'  : ['\\'])
                           ["\\\"", "\\n", "\\\\"]

replace :: (a -> Bool) -> [a] -> [a] -> [a]
replace _ _ [] = []
replace p a ls = l ++ r'
  where (l, r) = break p ls
        r' = if null r then r else a ++ replace p a (drop 1 r)

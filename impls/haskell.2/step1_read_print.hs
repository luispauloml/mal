import System.IO (hFlush, stdout, isEOF)

import Types (LispVal(..))
import Reader (read_str)
import Printer (pr_str)

lispRead :: String -> Maybe LispVal
lispRead = read_str

lispEval :: LispVal -> LispVal
lispEval = id

lispPrint :: LispVal -> String
lispPrint = pr_str

rep :: String -> String
rep str = maybe "error: no parse" (lispPrint . lispEval) (lispRead str)

repl :: IO ()
repl = do
  putStr "user> "
  hFlush stdout
  done <- isEOF
  if done
    then return ()
    else do line <- getLine
            if line == "quit"
              then return ()
              else do putStrLn $ rep line
                      repl

main :: IO ()
main = repl

import System.IO (hFlush, stdout, isEOF)

lispRead :: String -> String
lispRead = id

lispEval :: String -> String
lispEval = id

lispPrint :: String -> String
lispPrint = id

rep :: String -> String
rep = lispPrint . lispEval . lispRead

repl :: IO ()
repl = do
  putStr "user> "
  hFlush stdout
  done <- isEOF
  if done
    then return ()
    else do getLine >>= putStrLn . rep
            repl

main :: IO ()
main = repl

import qualified Data.Map as Map
import Type
import Core
import Analyzer
import Parser
import System.IO

globalEnv = coreEnv

eval :: Exp -> Env -> (Exp, Env)
eval exp = analyze exp

example :: Exp
example =
  let (_, env) = eval (parse line1) globalEnv
      (result, _) = eval (parse line2)  env
  in result
  where
    line1 = "(def sum-to-10 (fn (i result)                       \
            \                 (if (<= i 10)                      \
            \                   (sum-to-10 (+ i 1) (+ result i)) \
            \                   result)))"
    line2 = "(sum-to-10 0 0)"

repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == "quit"
    then do
    putStrLn "Bye See you soon!"
    return ()
    else do
      let (result, newEnv) = eval (parse line) env
      putStrLn $ show result
      hFlush stdout
      repl newEnv
  
main :: IO()
main = do
  putStrLn "Kand REPL"
  putStrLn "To exit type quit"
  repl globalEnv


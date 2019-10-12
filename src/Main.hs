import qualified Data.Map as Map
import Type
import Core
import Analyzer
import Parser
import System.IO

globalEnv = coreEnv

eval :: Exp -> Env -> (Exp, Env)
eval exp = analyze exp

repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == ":quit"
    then do
    putStrLn "Bye See you soon!"
    return ()
    else do
      let (result, newEnv) =
            foldl (\(_, env) exp -> eval exp env) (Unit, env) (parse line)
      putStrLn $ show result
      hFlush stdout
      repl newEnv
  
main :: IO()
main = do
  putStrLn "Kand REPL"
  putStrLn "To exit type :quit"
  repl globalEnv


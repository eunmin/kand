import qualified Data.Map as Map
import Type
import Core
import Analyzer
import Parser
import System.IO
import Compiler

globalEnv = coreEnv

eval :: Exp -> Env -> (Exp, Env)
eval exp = analyze exp

repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  line <- getLine
  let (command:args) = words line
  case command of
    ":quit" -> do putStrLn "Bye See you soon!"
                  return ()
    ":compile" -> do compile args
                     repl env
    _ -> do let (result, newEnv) = foldl (\(_, env) exp -> eval exp env) (Unit, env) (parse line)
            putStrLn $ show result
            hFlush stdout
            repl newEnv
  
main :: IO()
main = do
  putStrLn "Kand REPL"
  putStrLn "To exit type :quit"
  repl globalEnv


module Main where

import System.IO
import qualified Data.Map as Map
import Kand.Parser
import Kand.Analyzer

eval :: Expr -> Env -> (Either String KandType, Env)
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
    _ -> case (parse line) of
              Left error -> do putStrLn $ show error
                               hFlush stdout
                               repl env
              Right exprs -> do let (result, newEnv) =
                                      foldl (\(_, env) exp -> eval exp env) (Right KandUnit, env) exprs
                                case result of
                                  Left error -> do putStrLn $ show error
                                                   hFlush stdout
                                                   repl env
                                  Right value -> do putStrLn $ show value
                                                    hFlush stdout
                                                    repl newEnv

main :: IO()
main = do
  putStrLn "Kand REPL"
  putStrLn "To exit type :quit"
  repl $ Env (Map.fromList []) EmptyEnv

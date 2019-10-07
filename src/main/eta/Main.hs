module Main where

import qualified Data.Map as Map
import Type
import Core (plus, minus)
import Evaluator (eval)

globalEnv = Map.fromList [
  ("+", Fn plus),
  ("-", Fn minus),
  ("x", Nm 1)
  ]

main =
  putStrLn $ show (eval globalEnv expression)
  where
    -- (+ 10 (- 100 10)) ;; 100
    expression = Lst [Sym "+", Nm 10, Lst [Sym "-", Nm 100, Nm 10]]

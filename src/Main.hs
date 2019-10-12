import qualified Data.Map as Map
import Type
import Core
import Analyzer
import Parser

globalEnv = coreEnv

eval :: Exp -> Env -> (Exp, Env)
eval exp = analyze exp

main :: IO()
main = do
  putStrLn $ show result
  where
    line1 = "(def sum-to-10 (fn (i result)                       \
            \                 (if (<= i 10)                      \
            \                   (sum-to-10 (+ i 1) (+ result i)) \
            \                   result)))"
    line2 = "(sum-to-10 0 0)"
    (_, env) = eval (parse line1) globalEnv
    (result, _) = eval (parse line2)  env

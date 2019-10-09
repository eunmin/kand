import qualified Data.Map as Map
import Type
import Core
import Analyzer

globalEnv = EmptyEnv

eval :: Exp -> Env -> (Exp, Env)
eval exp = analyze exp

main :: IO()
main = do
  putStrLn $ show result
  where
    defPlus = Def (Sym "sumTo") (Lambda [Sym "x", Sym "y"] (
                               Application [Primitive plus, Sym "x", Sym "y"]
                               )
                           )
    -- (def - [x y] (primitive::minus x y))              
    defMinus = Def (Sym "-") (Lambda [Sym "x", Sym "y"] (
                                Application [Primitive minus, Sym "x", Sym "y"]
                                )
                            )
    -- (+ 10 (- 100 10)) ;; 100
    code = Application [Sym "+", Nm 10, Application [Sym "-", Nm 100, Nm 10]]
    -- (def sum-to-10 [i result]
    --    (if (<= i 10)
    --      (sum-to-10 (+ i 1) (+ result i))
    --      result))
    defSumTo10 = Def (Sym "sum-to-10") (
      (Lambda [Sym "i", Sym "result"] (
          (If (Application [Primitive le, Sym "i", Nm 10]) (
              Application [Sym "sum-to-10", Application [Primitive plus, Sym "i", Nm 1], Application [Primitive plus, Sym "result", Sym "i"]])
              (Sym "result")
          )
          )
      )
      )
    code2 = Application [Sym "sum-to-10", Nm 0, Nm 0]
--    (_, env1) = eval defPlus globalEnv
--    (_, env2) = eval defMinus env1
    (_, env) = eval defSumTo10 globalEnv
    (result, _) = eval code2 env

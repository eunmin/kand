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
    -- (def + [x y] (primitive::plus x y))
    defPlus = Def (Sym "+") (Lambda [Sym "x", Sym "y"] (
                               Application [(Primitive plus), Sym "x", Sym "y"]
                               )
                           )
    -- (def - [x y] (primitive::minus x y))              
    defMinus = Def (Sym "-") (Lambda [Sym "x", Sym "y"] (
                                Application [(Primitive minus), Sym "x", Sym "y"]
                                )
                            )
    -- (+ 10 (- 100 10)) ;; 100
    code = Application [Sym "+", Nm 10, Application [Sym "-", Nm 100, Nm 10]] 
    (_, env1) = eval defPlus globalEnv
    (_, env2) = eval defMinus env1
    (result, _) = eval code env2

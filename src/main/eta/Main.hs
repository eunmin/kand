import qualified Data.Map as Map

data Exp = Nm Double
          | Fn ([Exp] -> Exp)
          | Sym String
          | Lst [Exp]

type Env = Map.Map String Exp

plus :: [Exp] -> Exp
plus ((Nm x):(Nm y):[]) = Nm (x + y)

minus :: [Exp] -> Exp
minus ((Nm x):(Nm y):[]) = Nm (x - y)

eval :: Env -> Exp -> Exp
eval env (Lst ((Fn f):args)) = f $ map (eval env) args
eval env (Sym name) =
  case x of
    Just value -> value
  where
    x = Map.lookup name env
    
eval _ x = x

instance Show Exp where
  show = showExp

showExp :: Exp -> String
showExp (Nm n) = show n
showExp (Fn f) = "function"
showExp (Lst _) = "list"

globalEnv = Map.fromList [("x", Nm 100)]

main =
  putStrLn $ show (eval globalEnv expression)
  where
    -- (+ 10 (- x 10)) ;; 100
    expression = Lst [(Fn plus), Nm 1, Lst [(Fn minus), Sym "x", Nm 1]]

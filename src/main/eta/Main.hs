import qualified Data.Map as Map

data Exp = Nm Double
          | Fn ([Exp] -> Exp)
          | Sym String
          | Lst [Exp]
          | Error String 

type Env = Map.Map String Exp

instance Semigroup Exp where
  Error x <> Error y = Error (x ++ " " ++ y)

instance Monoid Exp where
  mempty = Error ""

plus :: [Exp] -> Exp
plus ((Nm x):(Nm y):[]) = Nm (x + y)
plus _ = Error "Invalid args"

minus :: [Exp] -> Exp
minus ((Nm x):(Nm y):[]) = Nm (x - y)
minus _ = Error "Invalid args"

eval :: Env -> Exp -> Exp
eval env (Lst (exp:exps)) =
  if length errors == 0
  then
    case exp of
      Fn f -> f args
      Sym name -> case (eval env (Sym name)) of
        Fn f -> f args
        _ -> Error $ (show exp) ++ " is not function"
      _ -> Error $ (show exp) ++ " is not function"
  else
    foldl (<>) mempty errors
  where
    args = map (eval env) exps
    errors = filter (\exp -> case exp of { (Error _) -> True; _ -> False }) args

eval env (Sym name) =
  case x of
    Just value -> value
    Nothing -> Error ("Unable to resolve symbol " ++ name)
  where
    x = Map.lookup name env
    
eval _ x = x

instance Show Exp where
  show = showExp

showExp :: Exp -> String
showExp (Nm n) = show n
showExp (Fn f) = "function"
showExp (Lst _) = "list"
showExp (Sym name) = "symbol " ++ name
showExp (Error message) = "Error: " ++ message

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

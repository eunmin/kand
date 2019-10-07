module Evaluator (
  eval
  ) where

import qualified Data.Map as Map
import Type

type Env = Map.Map String Exp

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

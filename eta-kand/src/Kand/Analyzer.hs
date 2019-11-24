module Kand.Analyzer ( Env(..)
                     , KandType(..)
                     , analyze ) where

import Kand.Parser
import Kand.Type
import Kand.Env

-- executeApplication :: Expr -> [Expr] -> Env -> (Expr, Env)

-- executeApplication (Lambda parameters body) args env =
--   bproc $ extendEnv parameters args env
--   where
--     bproc = analyze body

-- executeApplication (Primitive f) args env = (f args, env)

analyze :: Expr -> Env -> (Either String KandType, Env)

analyze (ExprList (ExprDef:(ExprSymbol name):expr:_)) = \env ->
  let (result, newEnv) = analyze expr env
  in
    case result of
      Left s -> (Left s, env)
      Right value -> (Right KandUnit, insertValue name value newEnv)
      
analyze (ExprSymbol name) = \env ->
  let value = lookupValue name env
  in case value of
    Just x -> (Right x, env)
    Nothing -> (Left ("Unable to resolve symbol: " ++ name ++ " in this context"), env)

analyze ExprTrue = \env -> (Right (KandBool True), env)

analyze ExprFalse = \env -> (Right (KandBool False), env)

analyze (ExprString s) = \env -> (Right (KandString s), env)

analyze (ExprNum n) = \env -> (Right (KandNumber n), env)

analyze expr = \env -> (Left ("Unable to resolve expression: " ++ show expr), env)

-- analyze (Application (operator:operands)) =
--   \env ->
--      let (app, _) = (fproc env)
--          args = map fst (map (\aproc -> aproc env) aprocs)
--      in executeApplication app args env
--   where
--     fproc = analyze operator
--     aprocs = map analyze operands

-- analyze (Sym name) = \env -> (lookupValue name env, env)

-- analyze (Def (Sym name) exp) = \env -> (Unit, insertValue name exp env)

-- analyze (If predicate consequent alternative) =
--   \env ->
--     let ((Boolean p), _) = (pproc env)
--     in if p then cproc env else aproc env
--   where
--     pproc = analyze predicate
--     cproc = analyze consequent
--     aproc = analyze alternative
  
-- analyze x = \env -> (x, env)

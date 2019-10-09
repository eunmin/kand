module Analyzer (analyze) where

import Type

executeApplication :: Exp -> [Exp] -> Env -> (Exp, Env)

executeApplication (Lambda parameters body) args env =
  bproc $ extendEnv parameters args env
  where
    bproc = analyze body

executeApplication (Primitive f) args env = (f args, env)

analyze :: Exp -> Env -> (Exp, Env)
analyze (Application (operator:operands)) =
  \env ->
     let (app, _) = (fproc env)
         args = map fst (map (\aproc -> aproc env) aprocs)
     in executeApplication app args env
  where
    fproc = analyze operator
    aprocs = map analyze operands

analyze (Sym name) = \env -> (lookupValue name env, env)

analyze (Def (Sym name) exp) = \env -> (Unit, insertValue name exp env)

analyze (If predicate consequent alternative) =
  \env ->
    let ((Boolean p), _) = (pproc env)
    in if p then cproc env else aproc env
  where
    pproc = analyze predicate
    cproc = analyze consequent
    aproc = analyze alternative
  
analyze x = \env -> (x, env)

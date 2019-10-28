module Core ( coreEnv ) where

import qualified Data.Map as Map
import Type

plus :: [Exp] -> Exp
plus ((Nm x):(Nm y):[]) = Nm (x + y)
plus _ = Error "Invalid args"

minus :: [Exp] -> Exp
minus ((Nm x):(Nm y):[]) = Nm (x - y)
minus _ = Error "Invalid args"

lt :: [Exp] -> Exp
lt ((Nm x):(Nm y):[]) = Boolean (x < y)

le :: [Exp] -> Exp
le ((Nm x):(Nm y):[]) = Boolean (x <= y)

gt :: [Exp] -> Exp
gt ((Nm x):(Nm y):[]) = Boolean (x > y)

ge :: [Exp] -> Exp
ge ((Nm x):(Nm y):[]) = Boolean (x >= y)

eq :: [Exp] -> Exp
eq ((Nm x):(Nm y):[]) = Boolean (x == y)

coreEnv :: Env
coreEnv = Env (Map.fromList
               [ ("<", Primitive lt)
               , ("<=", Primitive le)
               , (">", Primitive gt)
               , (">=", Primitive ge)
               , ("==", Primitive eq)
               , ("+", Primitive plus)
               , ("-", Primitive minus)
               ]) EmptyEnv

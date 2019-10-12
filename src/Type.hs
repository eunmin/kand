module Type (Exp(..)
            , Env(..)
            , extendEnv
            , lookupValue
            , insertValue) where

import qualified Data.Map as Map

type Frame = Map.Map String Exp

data Env = EmptyEnv
         | Env Frame Env
         deriving (Show)


data Exp = Nm Double
          | Boolean Bool
          | Lambda [Exp] Exp
          | Primitive ([Exp] -> Exp)
          | Sym String
          | Def Exp Exp
          | Application [Exp]
          | If Exp Exp Exp
          | Error String
          | Unit

instance Semigroup Exp where
  Error x <> Error y = Error (x ++ " " ++ y)

instance Monoid Exp where
  mempty = Error ""

instance Show Exp where
  show = showExp

showExp :: Exp -> String
showExp (Nm n) = "Number " ++ show n
showExp (Lambda args body) =
  "Lambda [" ++ unwords (map show args) ++ "] " ++ show body
showExp (Def name body) = "Def " ++ show name ++ " " ++ show body
showExp (Application exps) = "Appplication [" ++ unwords (map show exps) ++ "]"
showExp (Primitive _) = "primitive"
showExp (Sym name) = "Sym \"" ++ name ++ "\""
showExp (Boolean b) = show b
showExp (Error message) = "Error: " ++ message
showExp (If pred cons alter) = "If " ++ show pred ++ " " ++ show cons ++ " " ++ show alter
showExp Unit = "unit"

lookupValue :: String -> Env -> Exp
lookupValue var (Env frame parent) =
  case value of
    Just x -> x
    Nothing -> lookupValue var parent
  where
    value = Map.lookup var frame

lookupValue var EmptyEnv = Error $ var ++ "is not found"
  
extendEnv :: [Exp] -> [Exp] -> Env -> Env
extendEnv parameters args baseEnv =
  Env (Map.fromList (zip (map (\(Sym name) -> name) parameters) args)) baseEnv

insertValue :: String -> Exp -> Env -> Env
insertValue var exp (Env frame parent) = Env (Map.insert var exp frame) parent

insertValue var exp EmptyEnv = Env (Map.fromList [(var, exp)]) EmptyEnv


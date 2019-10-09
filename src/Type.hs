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
          | Lambda [Exp] Exp
          | Primitive ([Exp] -> Exp)
          | Sym String
          | Def Exp Exp
          | Application [Exp]
          | Error String
          | Unit

instance Semigroup Exp where
  Error x <> Error y = Error (x ++ " " ++ y)

instance Monoid Exp where
  mempty = Error ""

instance Show Exp where
  show = showExp

showExp :: Exp -> String
showExp (Nm n) = show n
showExp (Lambda _ _) = "lambda"
showExp (Def _ _) = "def"
showExp (Application _) = "application"
showExp (Primitive _) = "primitive"
showExp (Sym name) = "symbol " ++ name
showExp (Error message) = "Error: " ++ message

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


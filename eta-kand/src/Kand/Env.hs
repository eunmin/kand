module Kand.Env ( Env(..)
           , lookupValue
           , extendEnv
           , insertValue ) where

import qualified Data.Map as Map
import Kand.Type

type Frame = Map.Map String KandType

data Env = EmptyEnv
         | Env Frame Env
         deriving (Show)

lookupValue :: String -> Env -> Maybe KandType
lookupValue name (Env frame parent) =
  case value of
    Just x -> Just x
    Nothing -> lookupValue name parent
  where
    value = Map.lookup name frame

lookupValue _ EmptyEnv = Nothing    

extendEnv :: [String] -> [KandType] -> Env -> Env
extendEnv keys values baseEnv =
  Env (Map.fromList (zip keys values)) baseEnv

insertValue :: String -> KandType -> Env -> Env
insertValue name exp (Env frame parent) = Env (Map.insert name exp frame) parent

insertValue name exp EmptyEnv = Env (Map.fromList [(name, exp)]) EmptyEnv


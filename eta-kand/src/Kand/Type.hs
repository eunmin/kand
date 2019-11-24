module Kand.Type ( KandType(..) ) where

data KandType = KandList [KandType]
              | KandSymbol String
              | KandNumber Integer
              | KandString String
              | KandBool Bool
              | KandUnit
              deriving (Show)


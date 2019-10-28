{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds
, TypeOperators
, MultiParamTypeClasses
#-}

module Asm.AsmVar ( AsmVarArray(..)
                  , newAsmVar ) where

import Java
import Asm.AsmExp

data AsmVar = AsmVar @kand.asm.Var deriving Class

data AsmVarArray = AsmVarArray @kand.asm.Var[] deriving Class

type instance Inherits AsmVar = '[AsmExp]

instance JArray AsmVar AsmVarArray

type instance Inherits AsmVarArray = '[AsmExpArray]

foreign import java unsafe "@new" newAsmVar :: Int -> Java a AsmVar

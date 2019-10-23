{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

module Asm.AsmVar ( newAsmVar ) where

import Java
import Asm.AsmExp

data AsmVar = AsmVar @kand.asm.Var deriving Class

type instance Inherits AsmVar = '[AsmExp]

foreign import java unsafe "@new" newAsmVar :: (b <: AsmExp) => Int -> Java a b

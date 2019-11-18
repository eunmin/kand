{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DataKinds #-}

module Asm.AsmVar ( newAsmVar ) where

import Java
import Asm.AsmExp

data AsmVar = AsmVar @kand.asm.Var deriving Class

foreign import java unsafe "@new" newAsmVar :: Int -> Java a AsmVar

type instance Inherits AsmVar = '[AsmExp]

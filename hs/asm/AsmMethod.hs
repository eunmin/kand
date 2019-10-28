{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

module Asm.AsmMethod ( newAsmMethod ) where

import Java
import Asm.AsmExp
import Asm.AsmVar

data AsmMethod = AsmMethod @kand.asm.Method deriving Class

type instance Inherits AsmMethod = '[AsmExp]
                                
foreign import java unsafe "@new" newAsmMethod :: String -> String -> AsmExpArray -> Java a AsmMethod

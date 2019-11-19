{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

module Asm.AsmMethod ( newAsmMethod ) where

import Java
import Asm.AsmExp

data AsmMethod = AsmMethod @kand.asm.Method deriving Class

foreign import java unsafe "@new" newAsmMethod :: String -> String -> AsmExpArray -> Java a AsmMethod

type instance Inherits AsmMethod = '[AsmExp]

{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

module Asm.AsmNm ( newAsmNm ) where

import Java
import Asm.AsmExp

data AsmNm = AsmNm @kand.asm.Nm deriving Class

type instance Inherits AsmNm = '[AsmExp]

foreign import java unsafe "@new" newAsmNm :: Double -> Java a AsmNm

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DataKinds #-}

module Asm.AsmNm ( newAsmNm ) where

import Java
import Asm.AsmExp

data AsmNm = AsmNm @kand.asm.Nm deriving Class

foreign import java unsafe "@new" newAsmNm :: Double -> Java a AsmNm

type instance Inherits AsmNm = '[AsmExp]

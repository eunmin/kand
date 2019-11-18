{-# LANGUAGE NoImplicitPrelude, MagicHash, TypeOperators,
  DataKinds, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}

module Asm.AsmExp ( AsmExp(..)
                  , AsmExpArray(..) ) where

import Java
import Java.Array

data AsmExp = AsmExp @kand.asm.Exp deriving Class

data AsmExpArray = AsmExpArray @kand.asm.Exp[] deriving Class

foreign import java unsafe "@interface" visit :: MethodVisitor -> Java a ()

instance JArray AsmExp AsmExpArray

{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

module Asm.AsmExp ( AsmExp(..)
                  , AsmExpArray(..) ) where

import Java

data AsmExp = AsmExp @kand.asm.Exp deriving Class

data AsmExpArray = AsmExpArray @kand.asm.Exp[] deriving Class

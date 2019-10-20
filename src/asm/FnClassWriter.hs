{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

module Asm.FnClassWriter ( write ) where

import Java

data FnClassWriter = FnClassWriter @kand.asm.FnClassWriter deriving Class

foreign import java unsafe "@static kand.asm.FnClassWriter.write" write :: String -> JStringArray -> JByteArray

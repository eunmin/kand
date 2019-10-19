{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

module Asm.MainClassWriter ( write ) where

import Java

data MainClassWriter = MainClassWriter @kand.asm.MainClassWriter deriving Class

foreign import java unsafe "@static kand.asm.MainClassWriter.write" write :: String -> JByteArray

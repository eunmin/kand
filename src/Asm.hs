{-# LANGUAGE FlexibleContexts, DataKinds, TypeFamilies #-}

module Asm ( newClassWriter
           , visit
           , visitEnd
           , visitSource
           , toByteArray ) where

import Java

data ClassWriter = ClassWriter @org.objectweb.asm.ClassWriter deriving Class

foreign import java unsafe "@new" newClassWriter :: Int -> Java a ClassWriter

foreign import java unsafe visit :: Int -> Int -> String -> Maybe String -> String -> Maybe JStringArray -> Java ClassWriter ()

foreign import java unsafe visitEnd :: Java ClassWriter ()

foreign import java unsafe visitSource :: String -> Maybe String -> Java ClassWriter ()

foreign import java unsafe toByteArray :: Java ClassWriter JByteArray

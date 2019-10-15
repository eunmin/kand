{-# LANGUAGE FlexibleContexts, DataKinds, TypeFamilies #-}

module Asm.ClassWriter
  ( newClassWriter
  , visit
  , Asm.ClassWriter.visitEnd
  , visitSource
  , visitMethod
  , toByteArray ) where

import Java
import Asm.MethodVisitor

data ClassWriter = ClassWriter @org.objectweb.asm.ClassWriter deriving Class

foreign import java unsafe "@new" newClassWriter :: Int -> Java a ClassWriter

foreign import java unsafe visit :: Int -> Int -> String -> Maybe String -> String -> Maybe JStringArray -> Java ClassWriter ()

foreign import java unsafe visitEnd :: Java ClassWriter ()

foreign import java unsafe visitSource :: String -> Maybe String -> Java ClassWriter ()

foreign import java unsafe toByteArray :: Java ClassWriter JByteArray

foreign import java unsafe visitMethod :: Int -> String -> String -> Maybe String -> Maybe JStringArray -> Java ClassWriter MethodVisitor

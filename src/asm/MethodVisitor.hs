{-# LANGUAGE MagicHash, FlexibleContexts, TypeFamilies, DataKinds, TypeOperators #-}

module Asm.MethodVisitor
  ( MethodVisitor(..)
  , visitVarInsn
  , visitMethodInsn
  , visitInsn
  , visitMaxs
  , visitFieldInsn
  , visitLdcInsn
  , visitEnd ) where

import Java

data MethodVisitor = MethodVisitor @org.objectweb.asm.MethodVisitor deriving Class

foreign import java unsafe visitVarInsn :: Int -> Int -> Java MethodVisitor ()

foreign import java unsafe visitMethodInsn :: Int -> String -> String -> String -> Bool -> Java MethodVisitor ()

foreign import java unsafe visitInsn :: Int -> Java MethodVisitor ()

foreign import java unsafe visitMaxs :: Int -> Int -> Java MethodVisitor ()

foreign import java unsafe visitEnd :: Java MethodVisitor ()

foreign import java unsafe visitFieldInsn :: Int -> String -> String -> String -> Java MethodVisitor ()

foreign import java unsafe visitLdcInsn :: (a <: Object) => a -> Java MethodVisitor ()






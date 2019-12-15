{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}

module Kand.Reflect ( invoke
                    , newObject ) where

import Java

foreign import java unsafe "@static org.apache.commons.lang3.ClassUtils.getClass" getClassWithName :: String -> Java c (JClass a)

foreign import java unsafe "@static org.apache.commons.lang3.reflect.ConstructorUtils.invokeConstructor" newJavaObject :: (a <: Object) => JClass a -> JObjectArray -> Java c a

foreign import java unsafe "@static org.apache.commons.lang3.reflect.MethodUtils.invokeExactMethod" invokeMethod :: (a <: Object, b <: Object, c <: Object) => a -> String ->  JObjectArray -> (JClassArray c) -> Java d b

data {-# CLASS "java.lang.Class[]" #-} JClassArray a = JClassArray (Object# (JClassArray a)) deriving Class

instance JArray (JClass a) (JClassArray a)

newObject :: (a <: Object) => String -> [Object] -> Java c a
newObject className args = do
  arr <- arrayFromList args
  (cls :: JClass Object) <- getClassWithName className
  obj <- newJavaObject cls arr
  return $ unsafeCast obj

invoke :: (a <: Object) => Object -> String -> [Object] -> [String] -> Java c a
invoke obj methodName args argClasses = do
  arr <- arrayFromList args
  (argTypes :: [JClass Object]) <- mapM getClassWithName argClasses
  typeArr <- arrayFromList argTypes
  r <- invokeMethod obj methodName arr typeArr
  return r

example1 :: String -> IO JInteger
example1 s = java $ do
  obj <- newObject "java.lang.String" [(superCast $ toJString s)]
  r <- invoke obj "indexOf" [ superCast $ (toJava ("a" :: String) :: JString) ] ["java.lang.String"]
  return r

example2 :: String -> IO JInteger
example2 s = java $ do
  obj <- newObject "java.lang.String" [(superCast $ toJString s)]
  r <- invoke obj "indexOf" [ superCast $ (toJava (65 :: Int) :: JInteger) ] ["int"]
  return r

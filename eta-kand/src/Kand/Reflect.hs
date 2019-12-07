{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Kand.Reflect ( forName
                    , newObject
                    , invokeMethod
                    , invoke
                    , test
                    , test2
                    , newBoolean
                    , booleanValue
                    , newInstance ) where

import Java

foreign import java unsafe "@static java.lang.Class.forName" forName :: String -> Java c (JClass a)

foreign import java unsafe "@static org.apache.commons.lang3.reflect.ConstructorUtils.invokeConstructor" newObject :: (a <: Object) => JClass a -> JObjectArray -> Java c a

foreign import java unsafe "@static org.apache.commons.lang3.reflect.MethodUtils.invokeExactMethod" invokeMethod :: (a <: Object, b <: Object) => a -> String ->  JObjectArray -> Java c b

foreign import java unsafe isEmpty :: Java JString Bool

data JBool = JBool @java.lang.Boolean deriving Class

foreign import java unsafe "@new" newBoolean :: Bool -> Java a JBool

foreign import java unsafe booleanValue :: Java JBool Bool

type instance Inherits JBool = '[Object]

newInstance :: (a <: Object) => String -> [Object] -> Java c a
newInstance className args = do
  arr <- arrayFromList args
  (cls :: JClass Object) <- forName className
  obj <- newObject cls arr
  return $ unsafeCast obj

invoke :: (a <: Object) => Object -> String -> [Object] -> Java c a
invoke obj methodName args = do
  arr <- arrayFromList args
  r <- invokeMethod obj methodName arr
  return r

test :: String -> IO Bool
test s = java $ do
  obj <- newInstance "java.lang.String" [(superCast $ toJString s)]
  r <- invoke obj "isEmpty" []
  r2 <- r <.> booleanValue
  return r2

test2 :: String -> IO JInteger
test2 s = java $ do
  obj <- newInstance "java.lang.String" [(superCast $ toJString s)]
  r <- invoke obj "indexOf" [ superCast $ (toJava (0 :: Int) :: JInteger) ]
  return r

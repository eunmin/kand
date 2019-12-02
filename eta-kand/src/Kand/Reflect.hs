{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Kand.Reflect ( forName
                    , newObject
                    , invokeMethod
                    , test
                    , booleanValue
                    ) where

import Java

foreign import java unsafe "@static java.lang.Class.forName" forName :: String -> Java c (JClass a)

foreign import java unsafe "@static org.apache.commons.lang3.reflect.ConstructorUtils.invokeConstructor" newObject :: (a <: Object) => JClass a -> JObjectArray -> Java c a

foreign import java unsafe "@static org.apache.commons.lang3.reflect.MethodUtils.invokeExactMethod" invokeMethod :: (a <: Object, b <: Object) => a -> String ->  JObjectArray -> Java c b

foreign import java unsafe isEmpty :: Java JString Bool

foreign import java unsafe booleanValue :: Java JBool Bool

data JBool = JBool @java.lang.Boolean deriving Class

type instance Inherits JBool = '[Object]

test :: String -> IO JBool
test s = java $ do
  (arr :: JObjectArray) <- arrayFromList [superCast $ toJString s]
  (cls :: JClass JString) <- forName "java.lang.String"
  obj <- newObject cls arr
  (args :: JObjectArray) <- (arrayFromList [])
  (o :: Object) <- invokeMethod obj "isEmpty" args
  return $ unsafeCast o
--  (jb :: JBool) <- 
--  b <- jb <.> booleanValue




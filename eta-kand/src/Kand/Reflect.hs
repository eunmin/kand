{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Kand.Reflect ( forName
                    , newObject
                    , invokeMethod
                    , test
                    , test2
                    , newBoolean
                    , booleanValue
                    , lastIndexOf ) where

import Java

foreign import java unsafe "@static java.lang.Class.forName" forName :: String -> Java c (JClass a)

foreign import java unsafe "@static org.apache.commons.lang3.reflect.ConstructorUtils.invokeConstructor" newObject :: (a <: Object) => JClass a -> JObjectArray -> Java c a

foreign import java unsafe "@static org.apache.commons.lang3.reflect.MethodUtils.invokeExactMethod" invokeMethod :: (a <: Object, b <: Object) => a -> String ->  JObjectArray -> Java c b

foreign import java unsafe isEmpty :: Java JString Bool

data JBool = JBool @java.lang.Boolean deriving Class

foreign import java unsafe "@new" newBoolean :: Bool -> Java a JBool

foreign import java unsafe booleanValue :: Java JBool Bool

type instance Inherits JBool = '[Object]

test :: String -> Java c JBool
test s = do
  (arr :: JObjectArray) <- arrayFromList [superCast $ toJString s]
  (cls :: JClass JString) <- forName "java.lang.String"
  obj <- newObject cls arr
  (args :: JObjectArray) <- (arrayFromList [])
  (o :: Object) <- invokeMethod obj "isEmpty" args
  return $ unsafeCast o

test2 :: String -> IO Bool
test2 s = java $ do
  b <- test s
  bb <- b <.> booleanValue
  return bb

lastIndexOf :: String -> String -> Int -> Java c JInteger
lastIndexOf s str fromIndex = do
  (arr :: JObjectArray) <- arrayFromList [superCast $ toJString s]
  (cls :: JClass JString) <- forName "java.lang.String"
  obj <- newObject cls arr
  (args :: JObjectArray) <- arrayFromList [ superCast $ toJString str  
                                          , superCast $ (toJava fromIndex :: JInteger) ]
  (o :: Object) <- invokeMethod obj "lastIndexOf" args
  return $ unsafeCast o


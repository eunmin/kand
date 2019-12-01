{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Kand.Reflect ( forName
                    , newObject
                    , invokeMethod
                    , test
                    ) where

import Java

foreign import java unsafe "@static java.lang.Class.forName" forName :: String -> Java c (JClass a)

foreign import java unsafe "@static org.apache.commons.lang3.reflect.ConstructorUtils.invokeConstructor" newObject :: (a <: Object) => JClass a -> JObjectArray -> Java c a

foreign import java unsafe "@static org.apache.commons.lang3.reflect.MethodUtils.invokeExtractMethod" invokeMethod :: (a <: Object, b <: Object) => a -> String ->  JObjectArray -> b

foreign import java unsafe isEmpty :: Java JString Bool

test :: String -> IO Bool
test s = java $ do
  (arr :: JObjectArray) <- arrayFromList [superCast $ toJString s]
  (cls :: JClass JString) <- forName "java.lang.String"
  obj <- newObject cls arr
  empty <- obj <.> isEmpty
  return empty



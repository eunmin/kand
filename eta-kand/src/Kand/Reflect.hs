{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Kand.Reflect ( forName
                    , newObject
                    , invokeMethod
                    ) where

import Java

foreign import java unsafe "@static java.lang.Class.forName" forName :: String -> Java c (JClass a)

foreign import java unsafe "@static org.apache.commons.lang3.reflect.ConstructorUtils.invokeConstructor" newObject :: (a <: Object) => JClass a -> JObjectArray -> Java c a

foreign import java unsafe "@static org.apache.commons.lang3.reflect.MethodUtils.invokeExtractMethod" invokeMethod :: (a <: Object, b <: Object) => a -> String ->  JObjectArray -> b
-- (MethodUtils/invokeExactMethod obj method-name (into-array (map to-java args)))

-- test :: IO JString
-- test = java $ do
--   (arr :: JObjectArray) <- arrayFromList [superCast $ toJString "abc"]
--   (cls :: JClass JString) <- forName "java.lang.String"
--   obj <- newObject cls arr
--   return obj



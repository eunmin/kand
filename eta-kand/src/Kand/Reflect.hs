{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Kand.Reflect ( forName
                    , newObject
                    , test
                    ) where

import Java

foreign import java unsafe "@static org.apache.commons.lang3.reflect.ConstructorUtils.invokeConstructor" newObject :: (a <: Object) => JClass a -> JObjectArray -> Java c a

foreign import java unsafe "@static java.lang.Class.forName" forName :: String -> Java c (JClass a)

test :: IO JString
test = java $ do
  (arr :: JObjectArray) <- arrayFromList [superCast $ toJString "abc"]
  (cls :: JClass JString) <- forName "java.lang.String"
  obj <- newObject cls arr
  return obj



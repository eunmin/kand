{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings #-}

module Kand.Reflect ( classForName
                    , newObject
                    , test ) where

import Java

data JavaClass = JavaClass @java.lang.Class deriving Class

foreign import java unsafe "@static java.lang.Class.forName" classForName :: String -> Java c JavaClass

foreign import java unsafe "@static org.apache.commons.lang3.reflect.Constructorutils.invokeConstructor" newObject :: (a <: Object) => JavaClass -> JObjectArray -> Java c a

type instance Inherits String = '[Object]

test :: IO ()
test = java $ do
  cls <- classForName "java.lang.String"
  args <- arrayFromList ["a"]
  obj <- newObject cls args
  return obj

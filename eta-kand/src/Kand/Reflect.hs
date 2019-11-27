{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts, OverloadedStrings #-}

module Kand.Reflect ( classForName
                    , newObject
                    , test ) where

import Java

foreign import java unsafe "@static org.apache.commons.lang3.reflect.Constructorutils.invokeConstructor" newObject :: (a <: Object) => JClass a -> JObjectArray -> Java c a

foreign import java unsafe "@static java.lang.Class.forName" forName :: String -> JClass a

type instance Inherits String = '[Object]

test :: IO ()
test = java $ do
    args <- arrayFromList ["a"]
    return $ newObject (forName "java.lang.String") args



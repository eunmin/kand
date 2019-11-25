{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, FlexibleContexts #-}

module Kand.Reflect ( newObject ) where

import Java

foreign import java unsafe "@static org.apache.commons.lang3.reflect.Constructorutils.invokeConstructor" newObject :: Class a -> JObjectArray -> Java a c

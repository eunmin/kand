{-# LANGUAGE TypeFamilies, DataKinds, ScopedTypeVariables #-}

module Compiler ( compile ) where

import qualified Data.ByteString as BS
import Asm.FnClassWriter as FnClass
import Asm.MainClassWriter as MainClass
import Asm.AsmExp
import Asm.AsmMethod
import Asm.AsmVar
import Asm.AsmNm
import Parser
import Java

toJStringArray :: [String] -> JStringArray
toJStringArray strs = toJava jstrings
  where jstrings = map toJava strs :: [JString]

compile :: [String] -> IO ()
compile (filename:[]) = do
  code <- byteArray
  BS.writeFile "example/add.class" (BS.pack (fromJava code))
  BS.writeFile "example/main.class" (BS.pack (fromJava $ MainClass.write "example/main"))
  return ()
  where
    byteArray = java $ do
      var1 <- newAsmVar 1
      var2 <- newAsmVar 2
      args <- arrayFromList [ (superCast var1 :: AsmExp)
                            , (superCast var2 :: AsmExp) ]
      exp <- newAsmMethod "kand/runtime/Core" "add" args
      return $ FnClass.write "example/add" (toJStringArray ["x", "y"]) exp

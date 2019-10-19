module Compiler ( compile ) where

import qualified Data.ByteString as BS
import Asm.FnClassWriter as FnClass
import Asm.MainClassWriter as MainClass
import Parser
import Java

toJStringArray :: [String] -> JStringArray
toJStringArray strs = toJava jstrings
  where jstrings = map toJava strs :: [JString]
  
compile :: [String] -> IO ()
compile (filename:[]) = do
  BS.writeFile "example/add.class" (BS.pack (fromJava $ FnClass.write "example/add"))
  BS.writeFile "example/main.class" (BS.pack (fromJava $ MainClass.write "example/main"))
  return ()
  

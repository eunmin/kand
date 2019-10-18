module Compiler ( compile ) where

import qualified Data.ByteString as BS
import Asm.ClassWriter as CW
import Asm.MethodVisitor as MV
import Parser
import Java

writePrintlnClass :: String -> Java c JByteArray
writePrintlnClass text = do 
  cw <- newClassWriter 0
  cw <.> visit 49 (1 + 32) "Hello" Nothing "java/lang/Object" Nothing
  cw <.> visitSource "Hello.java" Nothing
  
  mw <- cw <.> visitMethod 1 "<init>" "()V" Nothing Nothing
  mw <.> visitVarInsn 25 0
  mw <.> visitMethodInsn 183 "java/lang/Object" "<init>" "()V" False
  mw <.> visitInsn 177
  mw <.> visitMaxs 1 1
  mw <.> MV.visitEnd
  
  mw <- cw <.> visitMethod 9 "main" "([Ljava/lang/String;)V" Nothing Nothing
  mw <.> visitFieldInsn 178 "java/lang/System" "out" "Ljava/io/PrintStream;"
  mw <.> visitLdcInsn (toJString text)
  mw <.> visitMethodInsn 182 "java/io/PrintStream" "println" "(Ljava/lang/String;)V" False
  mw <.> visitInsn 177
  mw <.> visitMaxs 2 1
  mw <.> MV.visitEnd

  cw <.> CW.visitEnd

  cw <.> toByteArray

toJStringArray :: [String] -> JStringArray
toJStringArray strs = toJava jstrings
  where jstrings = map toJava strs :: [JString]
  
writeClass :: Java c JByteArray
writeClass = do
  cw <- newClassWriter 0
  cw <.> visit 49 (1 + 32) "AddFn" Nothing "java/lang/Object" (Just (toJStringArray ["kand/runtime/Fn"]))
  cw <.> visitSource "AddFn.java" Nothing

  mw <- cw <.> visitMethod 1 "invoke" "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" Nothing Nothing
--  mw <.> visitFieldInsn 178 "/lang/System" "out" "Ljava/io/PrintStream;"
--  mw <.> visitLdcInsn (toJString "")

  mw <.> visitVarInsn 25 1
  mw <.> visitVarInsn 25 2
  mw <.> visitMethodInsn 184 "kand/runtime/Core" "add" "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
  mw <.> visitInsn 176
  mw <.> visitMaxs 1 (-1)
  mw <.> MV.visitEnd

  cw <.> CW.visitEnd
  cw <.> toByteArray

compile :: [String] -> IO ()
compile (filename:[]) = do
--  contents <- readFile filename
--  putStrLn $ show (parse contents)
  byteCode <- java writeClass
  BS.writeFile "AddFn.class" (BS.pack (fromJava byteCode))
  return ()
  

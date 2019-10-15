import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Type
import Core
import Analyzer
import Parser
import System.IO
import Asm.ClassWriter as CW
import Asm.MethodVisitor as MV
import Java

globalEnv = coreEnv

eval :: Exp -> Env -> (Exp, Env)
eval exp = analyze exp

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

repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    ":quit" -> do putStrLn "Bye See you soon!"
                  return ()
    ":compile" -> do byteCode <- java (writePrintlnClass "Hello World")
                     putStrLn "Bye See you soon!"
                     BS.writeFile "Hello.class" (BS.pack (fromJava byteCode))
                     return ()
    _ -> do let (result, newEnv) = foldl (\(_, env) exp -> eval exp env) (Unit, env) (parse line)
            putStrLn $ show result
            hFlush stdout
            repl newEnv
  
main :: IO()
main = do
  putStrLn "Kand REPL"
  putStrLn "To exit type :quit"
  repl globalEnv


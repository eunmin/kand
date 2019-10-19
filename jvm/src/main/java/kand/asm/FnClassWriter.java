package kand.asm;

import org.objectweb.asm.*;
import static org.objectweb.asm.Opcodes.*;

public class FnClassWriter {
    public static void createBasicConstructor(ClassWriter cw, String name) {
        MethodVisitor mv;

        mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        Label label0 = new Label();
        mv.visitLabel(label0);
        mv.visitLineNumber(6, label0);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
        mv.visitInsn(RETURN);
        Label label1 = new Label();
        mv.visitLabel(label1);
        mv.visitLocalVariable("this", "L" + name + ";", null, label0, label1, 0);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
    }

    public static byte[] write(String name) {
        ClassWriter cw = new ClassWriter(0);
        MethodVisitor mv;

        cw.visit(V1_8, ACC_PUBLIC | ACC_SUPER, name, null, "java/lang/Object", new String[]{"kand/runtime/Fn"});
        cw.visitSource(Util.fileName(name), null);

        createBasicConstructor(cw, name);

        {
            mv = cw.visitMethod(ACC_PUBLIC, "invoke", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", null, null);
            mv.visitCode();
            Label label0 = new Label();
            mv.visitLabel(label0);
            mv.visitLineNumber(8, label0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitMethodInsn(INVOKESTATIC, "kand/runtime/Core", "add", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", false);
            mv.visitInsn(ARETURN);
            Label label1 = new Label();
            mv.visitLabel(label1);
            mv.visitLocalVariable("this", "Lexample/add;", null, label0, label1, 0);
            mv.visitLocalVariable("o1", "Ljava/lang/Object;", null, label0, label1, 1);
            mv.visitLocalVariable("o2", "Ljava/lang/Object;", null, label0, label1, 2);
            mv.visitMaxs(2, 3);
            mv.visitEnd();
        }
        cw.visitEnd();

        return cw.toByteArray();
    }
}

package kand.asm;

import org.objectweb.asm.*;
import static org.objectweb.asm.Opcodes.*;

public class MainClassWriter {
    public static byte[] write(String name) {
        ClassWriter classWriter = new ClassWriter(0);
        MethodVisitor methodVisitor;

        classWriter.visit(V1_8, ACC_PUBLIC | ACC_SUPER, name, null, "java/lang/Object", null);

        classWriter.visitSource(Util.fileName(name), null);

        {
            methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
            methodVisitor.visitCode();
            Label label0 = new Label();
            methodVisitor.visitLabel(label0);
            methodVisitor.visitLineNumber(3, label0);
            methodVisitor.visitVarInsn(ALOAD, 0);
            methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
            methodVisitor.visitInsn(RETURN);
            Label label1 = new Label();
            methodVisitor.visitLabel(label1);
            methodVisitor.visitLocalVariable("this", "L" + name + ";", null, label0, label1, 0);
            methodVisitor.visitMaxs(1, 1);
            methodVisitor.visitEnd();
        }
        {
            methodVisitor = classWriter.visitMethod(ACC_PUBLIC | ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
            methodVisitor.visitCode();
            Label label0 = new Label();
            methodVisitor.visitLabel(label0);
            methodVisitor.visitLineNumber(5, label0);
            methodVisitor.visitTypeInsn(NEW, "example/add");
            methodVisitor.visitInsn(DUP);
            methodVisitor.visitMethodInsn(INVOKESPECIAL, "example/add", "<init>", "()V", false);
            methodVisitor.visitTypeInsn(NEW, "java/lang/Double");
            methodVisitor.visitInsn(DUP);
            methodVisitor.visitLdcInsn(new Double("2.0"));
            methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Double", "<init>", "(D)V", false);
            methodVisitor.visitTypeInsn(NEW, "java/lang/Double");
            methodVisitor.visitInsn(DUP);
            methodVisitor.visitLdcInsn(new Double("3.0"));
            methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Double", "<init>", "(D)V", false);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "example/add", "invoke", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", false);
            methodVisitor.visitVarInsn(ASTORE, 1);
            Label label1 = new Label();
            methodVisitor.visitLabel(label1);
            methodVisitor.visitLineNumber(6, label1);
            methodVisitor.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            methodVisitor.visitVarInsn(ALOAD, 1);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V", false);
            Label label2 = new Label();
            methodVisitor.visitLabel(label2);
            methodVisitor.visitLineNumber(7, label2);
            methodVisitor.visitInsn(RETURN);
            Label label3 = new Label();
            methodVisitor.visitLabel(label3);
            methodVisitor.visitLocalVariable("args", "[Ljava/lang/String;", null, label0, label3, 0);
            methodVisitor.visitLocalVariable("result", "Ljava/lang/Object;", null, label1, label3, 1);
            methodVisitor.visitMaxs(6, 2);
            methodVisitor.visitEnd();
        }
        classWriter.visitEnd();

        return classWriter.toByteArray();
    }
}

package kand.asm;

import org.objectweb.asm.MethodVisitor;
import static org.objectweb.asm.Opcodes.INVOKESTATIC;

public class Nm implements Exp {

    private double value;

    public Nm(double value) {
        this.value = value;
    }

    @Override
    public void visit(MethodVisitor mv) {
        System.out.println("visitIntInsn " + value);
        mv.visitLdcInsn(new Double(value));
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false);
    }
}

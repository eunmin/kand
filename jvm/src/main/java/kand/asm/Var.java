package kand.asm;

import org.objectweb.asm.MethodVisitor;

import static org.objectweb.asm.Opcodes.ALOAD;

public class Var implements Exp {
    private int index;

    public Var(int index) {
        this.index = index;
    }

    @Override
    public void visit(MethodVisitor mv) {
        System.out.println("visitVarInsn " + index);
        mv.visitVarInsn(ALOAD, index);
    }
}

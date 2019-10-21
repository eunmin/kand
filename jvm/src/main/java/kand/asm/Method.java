package kand.asm;

import org.objectweb.asm.MethodVisitor;

import static org.objectweb.asm.Opcodes.INVOKESTATIC;

public class Method implements Exp {
    private String owner;
    private String name;
    private Exp[] args;

    public Method(String owner, String name, Exp[] args) {
        this.owner = owner;
        this.name = name;
        this.args = args;
    }

    @Override
    public void visit(MethodVisitor mv) {
        StringBuilder sb = new StringBuilder("(");
        for (int i = 0; i < args.length; i++) {
            args[i].visit(mv);
            sb.append("Ljava/lang/Object;");
        }
        sb.append(")Ljava/lang/Object;");
        String descriptor = sb.toString();
        System.out.println("visitMethodInsn " + owner + ", " + name + ", " + descriptor);
        mv.visitMethodInsn(INVOKESTATIC, owner, name, descriptor, false);
    }
}

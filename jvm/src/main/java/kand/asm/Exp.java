package kand.asm;

import org.objectweb.asm.MethodVisitor;

public interface Exp {
    void visit(MethodVisitor mv);
}

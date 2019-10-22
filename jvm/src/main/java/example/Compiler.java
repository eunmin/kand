package example;

import kand.asm.*;

import java.io.FileOutputStream;
import java.io.IOException;

public class Compiler {
    public static void writeFile(String filename, byte[] data) throws IOException {
        FileOutputStream out=new FileOutputStream(filename);
        out.write(data);
        out.close();
    }

    public static void main(String[] args) throws IOException {
        Method exp = new Method("kand/runtime/Core", "add",
                new Exp[] { new Nm(100), new Method("kand/runtime/Core", "add",
                        new Exp[] { new Var(1), new Var(2)})});

        writeFile("example/add.class", FnClassWriter.write("example/add", new String[] {"x", "y"}, exp));
        writeFile("example/main.class", MainClassWriter.write("example/main"));
    }
}

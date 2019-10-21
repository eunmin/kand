package example;

import kand.asm.FnClassWriter;
import kand.asm.MainClassWriter;

import java.io.FileOutputStream;
import java.io.IOException;

public class Compiler {
    public static void writeFile(String filename, byte[] data) throws IOException {
        FileOutputStream out=new FileOutputStream(filename);
        out.write(data);
        out.close();
    }

    public static void main(String[] args) throws IOException {
        writeFile("example/add.class", FnClassWriter.write("example/add", new String[] {"x", "y"}));
        writeFile("example/main.class", MainClassWriter.write("example/main"));
    }
}

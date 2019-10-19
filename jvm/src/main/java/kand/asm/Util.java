package kand.asm;

public class Util {
    public static String fileName(String name) {
        String [] parts = name.split("/");
        if (parts != null && parts.length > 0) {
            return parts[parts.length - 1] + ".java";
        }
        return null;
    }
}

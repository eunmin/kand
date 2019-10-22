package kand.runtime;

public class Core {
    public static Object add(Object x, Object y) {
        return new Double((Double)x + (Double)y);
    }
}

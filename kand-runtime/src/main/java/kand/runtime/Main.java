package kand.runtime;

public class Main {
    public static void main(String[] args) {
        Object result = new ExampleFn().invoke(new Integer(1), new Integer(2));
        System.out.println(result);
    }
}

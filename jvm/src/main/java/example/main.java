package example;

public class main {
    public static void main(String[] args) {
        Object result = new add().invoke(new Integer(1), new Integer(2));
        System.out.println(result);
    }
}

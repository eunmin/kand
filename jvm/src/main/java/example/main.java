package example;

public class main {
    public static void main(String[] args) {
        Object result = new add().invoke(new Double(1), new Double(2));
        System.out.println(result);
    }
}

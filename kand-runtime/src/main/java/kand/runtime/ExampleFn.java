package kand.runtime;

public class ExampleFn implements Fn {
    public Object invoke(Object o1, Object o2) {
        return Core.add(o1, o2);
    }
}

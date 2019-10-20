package example;

import kand.runtime.Core;
import kand.runtime.Fn;

public class add implements Fn {
    public Object invoke(Object o1, Object o2) {;
        return Core.add(Core.add(Core.add(123, o2), o2), Core.add(o1,o2));
    }
}

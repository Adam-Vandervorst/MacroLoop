package macroloop.port;

import java.util.function.Function;

public class ArrayIndex {
    public static <T, U> void forEach(T[] a, Function<T, U> f) {
        int size = a.length;
        for (int i = 0; i < size; i++) {
            f.apply(a[i]);
        }
    }

    public static <T> Boolean forall(T[] a, Function<T, Boolean> f) {
        int size = a.length;
        for (int i = 0; i < size; i++) {
            if (!f.apply(a[i])) return false;
        }
        return true;
    }
}

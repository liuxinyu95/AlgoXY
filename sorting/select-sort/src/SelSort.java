import java.util.*;
import java.util.stream.*;
import java.lang.Exception;

public class SelSort {

    public static void cocktailSort(int[] xs) {
        int n = xs.length;
        int m = n / 2;
        for (int i = 0; i < m; ++i) {
            int min = i;
            int max = n - 1 - i;
            if (xs[max] < xs[min]) {
                swap(xs, min, max);
            }
            for (int j = i + 1; j < n - 1 - i; ++j) {
                if (xs[j] < xs[min]) {
                    min = j;
                }
                if (xs[max] < xs[j]) {
                    max = j;
                }
            }
            swap(xs, i, min);
            swap(xs, n - i - 1, max);
        }
    }

    static final void swap(int[] xs, int i, int j) {
        if (i != j) {
            int x = xs[i];
            xs[i] = xs[j];
            xs[j] = x;
        }
    }

    // verification

    final static int N = 20;//1000;

    static int[] fromList(List<Integer> xs) {
        return xs.stream().mapToInt(Integer::intValue).toArray();
    }

    static void assertEq(int[] as, int[] bs, int[] cs) {
        if (Arrays.compare(as, bs) != 0) {
            System.out.println("A: " + Arrays.toString(as) +
                               "\nB: " + Arrays.toString(bs) +
                               "\nC: " + Arrays.toString(cs));
            throw new RuntimeException("assert fail");
        }
    }

    static void test() {
        List<Integer> xs = IntStream.range(0, N).boxed().collect(Collectors.toList());
        Random gen = new Random();
        for (int i = 0; i < 100; i++) {
            Collections.shuffle(xs);
            int n = gen.nextInt(N);
            int[] as = fromList(xs.subList(0, n));
            int[] bs = Arrays.copyOf(as, n);
            int[] cs = Arrays.copyOf(as, n);
            cocktailSort(as);
            Arrays.sort(bs);
            assertEq(as, bs, cs);
        }
        System.out.println("passed 100 tests");
    }

    public static void main(String[] args) {
        test();
    }
}

import java.util.*;
import java.util.stream.*;

public class Comb {

    static class Tuple {
        List<Integer> xs;
        int i;

        Tuple(List<Integer> xs, int i) {
            this.xs = xs;
            this.i = i;
        }
    }

    // DFS method (without recursion) to generate combinations
    public static <T> List<List<T>> comb(final List<T> xs, final int r) {
        List<List<T>> res = new ArrayList<>();
        if (xs.size() < r || r <= 0) { return res; }
        Stack<Tuple> stack = new Stack<>();
        stack.push(new Tuple(Collections.emptyList(), 0));
        while(!stack.empty()) {
            Tuple t = stack.pop();
            if (t.xs.size() == r) {
                res.add(t.xs.stream().map(i -> xs.get(i)).collect(Collectors.toList()));
            } else if (t.xs.size() < r && t.i < xs.size()) {
                List<Integer> ys = new ArrayList<>(t.xs);
                ys.add(t.i++);
                stack.push(new Tuple(ys, t.i));
                stack.push(new Tuple(new ArrayList<>(t.xs), t.i));
            }
        }
        Collections.reverse(res);
        return res;
    }

    // DFS with recursion
    public static <T> List<List<T>> combination(final List<T> xs, final int r) {
        return comb(xs, r, 0, Collections.emptyList(), new ArrayList<>());
    }

    private static <T> List<List<T>> comb(final List<T> xs, final int r,
                                          final int i, final List<T> ys,
                                          List<List<T>> acc) {
        if (ys.size() == r)
            acc.add(ys);
        else {
            for (int j = i; j < xs.size(); ++j) {
                List<T> zs = new ArrayList<>(ys);
                zs.add(xs.get(j));
                comb(xs, r, j + 1, zs, acc);
            }
        }
        return acc;
    }

    public static void main(String[] args) {
        List<String> xs = Arrays.asList("a", "b", "c", "d");
        for (int r = 0; r < 6; ++r) {
            System.out.println(String.format("\nC_%d^%d:", xs.size(), r));
            System.out.println("DFS without recursion:");
            comb(xs, r).forEach(ys -> System.out.println(String.join(", ", ys)));
            System.out.println("DFS with recurion:");
            combination(xs, r).forEach(ys -> System.out.println(String.join(", ", ys)));
        }
    }
}

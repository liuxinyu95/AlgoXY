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

    // DFS method to generate combinations
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

    public static void main(String[] args) {
        List<String> xs = Arrays.asList("a", "b", "c", "d");
        for (int r = 0; r < 6; ++r) {
            System.out.println(String.format("C_%d^%d:", xs.size(), r));
            comb(xs, r).forEach(ys -> System.out.println(String.join(", ", ys)));
        }
    }
}

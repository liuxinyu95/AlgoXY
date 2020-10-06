import java.util.*;
import java.util.stream.*;
import java.lang.Exception;
import java.lang.Math;

public class IntTree {
    public static class Node<T> {
        int key;
        T value;
        int prefix;
        int mask;
        Node<T> left;
        Node<T> right;

        public Node(int k, T v) {
            key = k;
            value = v;
            prefix = k;
            mask = 1;
        }

        public Node() { this(0, null); }

        boolean isLeaf() {
            return left == null && right == null;
        }

        boolean match(int x) {
            return maskbit(x, mask) == prefix;
        }

        void replaceSubTree(Node<T> x, Node<T> y) {
            if (left == x) {
                left = y;
            } else {
                right = y;
            }
        }

        void setSubTrees(Node<T> l, Node<T> r) {
            left = l;
            right = r;
        }
    }

    static int maskbit(int x, int mask) {
        return x & (~(mask - 1));
    }

    static boolean zero(int x, int mask) {
        return (x & (mask >> 1)) == 0;
    }

    // find the longest common prefix, returns a pair (lcp, mask)
    static int[] lcp(int p1, int p2) {
        int diff = p1 ^ p2;
        int mask = 1;
        while (diff != 0) {
            diff >>= 1;
            mask <<= 1;
        }
        int prefix = maskbit(p1, mask);
        return new int[]{prefix, mask};
    }

    static <T> Node<T> branch(Node<T> t1, Node<T> t2) {
        Node<T> t = new Node<T>();
        int[] pair = lcp(t1.prefix, t2.prefix);
        t.prefix = pair[0];
        t.mask   = pair[1];
        if (zero(t1.prefix, t.mask)) {
            t.setSubTrees(t1, t2);
        } else {
            t.setSubTrees(t2, t1);
        }
        return t;
    }

    public static <T> Node<T> insert(Node<T> t, int key, T value) {
        if (t == null)
            return new Node<T>(key, value);

        Node<T> node = t, parent = null;
        while (!node.isLeaf() && node.match(key)) {
            parent = node;
            node = zero(key, node.mask) ? node.left : node.right;
        }
        if (node.isLeaf() && key == node.key) {
            node.value = value;
        } else {
            Node<T> p = branch(node, new Node<T>(key, value));
            if (parent == null)
                return p;
            parent.replaceSubTree(node, p);
        }
        return t;
    }

    public static <T> Optional<T> lookup(Node<T> t, int key) {
        while (t != null && !t.isLeaf() && t.match(key)) {
            t = zero(key, t.mask) ? t.left : t.right;
        }
        if (t != null && t.isLeaf() && t.key == key)
            return Optional.ofNullable(t.value);
        return Optional.empty();
    }

    //auxiliary methods
    public static <T> Node<T> fromMap(Map<Integer, T> m) {
        Node<T> t = null;
        for (Integer key : m.keySet()) {
            t = insert(t, key, m.get(key));
        }
        return t;
    }

    public static class Test {
        final static int N = 100;

        static Map<Integer, Integer> genMap(Random gen, int size) {
            Map<Integer, Integer> m = new HashMap<>(size);
            List<Integer> ks =  IntStream.range(0, N).boxed().collect(Collectors.toList());
            Collections.shuffle(ks);
            for (Integer k : ks.subList(0, size)) {
                m.put(k, gen.nextInt(N));
            }
            return m;
        }

        static <T> void testBuild(Map<Integer, T> m) {
            Node<T> t = fromMap(m);
            for (Integer k : m.keySet()) {
                T v = lookup(t, k).get();
                if (!v.equals(m.get(k)))
                    throw new RuntimeException(String.format("lookup key=%d, expect: %s, get: %s\n",
                                                             k, k.toString(), v.toString()));
            }
            for (int i = 0; i < N; ++i) {
                if (!m.containsKey(i) && lookup(t, i).isPresent())
                    throw new RuntimeException(String.format("lookup key=%d, expect: None, get: %s\n",
                                                             i, lookup(t, i).get().toString()));
            }
            System.out.format(".");
        }

        public static void test() {
            Random gen = new Random();
            for (int i = 0; i < N; ++i) {
                testBuild(genMap(gen, Math.max(gen.nextInt(N), 0)));
            }
            System.out.format("%d test passed.\n", N);
        }
    }
}

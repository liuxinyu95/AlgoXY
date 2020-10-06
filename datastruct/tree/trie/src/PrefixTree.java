/*
 * PrefixTree.java
 * Copyright (C) 2018 Liu Xinyu (liuxinyu95@gmail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
import java.util.*;
import java.util.stream.*;
import java.lang.Exception;

public class PrefixTree {
    public static class Node<T> {
        Optional<T> value = Optional.empty();
        Map<String, Node<T>> subTrees = new HashMap<>();

        public Node() {}

        public Node(T val) {
            value = Optional.of(val);
        }
    }

    public static <T> Node<T> leaf(T val) { return new Node<T>(val); }

    public static <T> Node<T> insert(Node<T> t, String key, T val) {
        if (t == null)
            t = new Node<T>();
        Node<T> p = t;
        for (;;) {
            String prefix = "";
            for (Map.Entry<String, Node<T>> entry : p.subTrees.entrySet()) {
                String k = entry.getKey();
                Node<T> tr = entry.getValue();
                if (k.equals(key)) { // overwrite
                    p.value = Optional.of(val);
                    return t;
                }
                String[] r = lcp(key, k);
                prefix = r[0];
                String k1 = r[1], k2 = r[2];
                if (!prefix.isEmpty()) {
                    if (k2.isEmpty()) {
                        // e.g. insert "another" into "an", go on traversing
                        p = tr;
                        key = k1;
                        break;
                    } else {
                        // branch out a new leaf
                        p.subTrees.put(prefix, branch(k1, leaf(val), k2, tr));
                        p.subTrees.remove(k);
                        return t;
                    }
                }
            }
            if (prefix.isEmpty()) {
                p.subTrees.put(key, leaf(val)); // add a new leaf
                break;
            }
        }
        return t;
    }

    /*
     * the longest common prefix
     * returns (p, s1', s2'), where p is LCP, s1' = s1 - p, s2' = s2 - p
     */
    static String[] lcp(String s1, String s2) {
        int i = 0, m = s1.length(), n = s2.length();
        while (i < m && i < n && s1.charAt(i) == s2.charAt(i))
            ++i;
        return new String[] {s1.substring(0, i), s1.substring(i), s2.substring(i)};
    }

    // branch
    static <T> Node<T> branch(String key1, Node<T> t1, String key2, Node<T> t2) {
        if (key1.isEmpty()) {    // e.g. insert "an" into "another"
            t1.subTrees.put(key2, t2);
            return t1;
        }
        Node<T> t = new Node<>();
        t.subTrees.put(key1, t1);
        t.subTrees.put(key2, t2);
        return t;
    }

    public static <T> Optional<T> lookup(Node<T> t, String key) {
        if (t == null)
            return Optional.empty();
        boolean match;
        do {
            match = false;
            for (Map.Entry<String, Node<T>> entry : t.subTrees.entrySet()) {
                String k = entry.getKey();
                Node<T> tr = entry.getValue();
                if (k.equals(key))
                    return tr.value;
                String[] r = lcp(key, k);
                String prefix = r[0],k1 = r[1], k2 = r[2];
                if (!prefix.isEmpty() && k2.isEmpty()) {
                    match = true;
                    key = k1;
                    t = tr;
                    break;
                }
            }
        } while (match);
        return Optional.empty();
    }

    public static List<String> keys(Node<?> t) { return keysOfPrefix(t, ""); }

    private static List<String> keysOfPrefix(Node<?> t, String prefix) {
        List<String> ks = new ArrayList<>();
        if (t.value.isPresent())
            ks.add(prefix);
        for (String key : new TreeSet<String>(t.subTrees.keySet())) {
            ks.addAll(keysOfPrefix(t.subTrees.get(key), prefix + key));
        }
        return ks;
    }

    // auxiliary methods
    public static <T> Node<T> fromMap(Map<String, T> m) {
        Node<T> t = null;
        for (String key : m.keySet()) {
            t = insert(t, key, m.get(key));
        }
        return t;
    }

    public static Node<Integer> fromString(String txt) {
        String[] words = txt.split("\\W+");
        Node<Integer> t = null;
        for (int i = 0; i < words.length; ++i)
            t = insert(t, words[i], i);
        return t;
    }

    public static class Test {
        final static int N = 100;
        final static List<String> testData =
            new ArrayList<>(Arrays.asList("a another an boy bool zoo",
                                          "zoo bool boy another an a",
                                          "zoo is a place where animals are for public to see"));

        static Map<String, Integer> genMap(Random gen, String txt) {
            Map<String, Integer> m = new HashMap<>();
            String[] words = txt.split("\\W+");
            for (String w : words) {
                m.put(w, gen.nextInt(N));
            }
            return m;
        }

        static String join(Collection<String> xs) {
            return xs.stream().collect(Collectors.joining(", "));
        }

        static void testBuild(Map<String, Integer> m) {
            Node<Integer> t = fromMap(m);
            for (String key : m.keySet()) {
                Optional<Integer> v = lookup(t, key);
                if (!v.isPresent())
                    throw new RuntimeException(String.format("not found: %s\n", key));
                if (!v.get().equals(m.get(key)))
                    throw new RuntimeException(String.format("lookup key=%s, expect: %d, get: %d\n",
                                                             key, m.get(key), v.get()));
            }
            List<String> ks1 = keys(t);
            List<String> ks2 = new ArrayList<>(m.keySet());
            Collections.sort(ks2);
            if (!ks1.equals(ks2))
                throw new RuntimeException(String.format("\nkeys1 = [%s]\n!= \nkeys2 = [%s]\n",
                                                         join(ks1), join(ks2)));
            System.out.format("populate keys: %s\n", join(ks1));
        }

        static void test() {
            Random gen = new Random();
            for (String txt : testData) {
                testBuild(genMap(gen, txt));
            }
            System.out.format("%d test passed.\n", testData.size());
        }
    }
}

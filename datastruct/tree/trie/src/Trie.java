/*
 * Trie.java
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

/*
 * Alphabetic Trie
 * limit key to lower case English chars (26) for illustration purpose
 */

public class Trie {
    public static class Node<T> {
        List<Node<T>> children = new ArrayList<>(Collections.nCopies(26, null));
        T value;
        public Node() {}
    }

    public static <T> Node<T> insert(Node<T> t, String key, T value) {
        if (t == null)
            t = new Node<T>();
        Node<T> p = t;
        int n = key.length();
        for (int i = 0; i < n; ++i) {
            int c = key.charAt(i) - 'a';
            if (p.children.get(c) == null) {
                p.children.set(c, new Node<T>());
            }
            p = p.children.get(c);
        }
        p.value = value;
        return t;
    }

    public static <T> Optional<T> lookup(Node<T> t, String key) {
        int n = key.length();
        for (int i = 0; i < n; ++i) {
            int c = key.charAt(i) - 'a';
            if (t.children.get(c) == null) {
                return Optional.empty();
            }
            t = t.children.get(c);
        }
        return Optional.ofNullable(t.value);
    }

    public static List<String> keys(Node<?> t) {
        return keysOfPrefix(t, "");
    }

    private static List<String> keysOfPrefix(Node<?> t, String prefix) {
        List<String> ks = new ArrayList<>();
        if (t.value != null)
            ks.add(prefix);
        int n = t.children.size();
        for (int i = 0; i < n; ++i) {
            if (t.children.get(i) != null) {
                ks.addAll(keysOfPrefix(t.children.get(i),
                                       prefix + Character.toString((char)('a' + i))));
            }
        }
        return ks;
    }

    //auxiliary methods
    public static <T> Node<T> fromMap(Map<String, T> m) {
        Node<T> t = null;
        for (String key : m.keySet()) {
            t = insert(t, key, m.get(key));
        }
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
                throw new RuntimeException(String.format("keys1 = [%s]\n!= \nkeys2 = [%s]\n",
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

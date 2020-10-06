/*
 * IntTrie.java
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
 */

/*
 * Int (as key) Radix Tree
 * Chris Okasaki and Andy Gill,  "Fast Mergeable Integer Maps",
 * Workshop on ML, September 1998, pages 77-86,
 */

import java.util.*;
import java.util.stream.*;
import java.lang.Exception;
import java.lang.Math;

public class IntTrie {

    public static class Node<T> {
        T value;
        Node<T> left;
        Node<T> right;

        public Node(T val) {
            value = val;
        }

        public Node() { this(null); }
    }

    public static <T> Node<T> insert(Node<T> t, int key, T val) {
        if (t == null)
            t = new Node<T>();
        Node<T> p = t;
        while (key != 0) {
            if (0 == (key & 0x1)) { //even
                if (p.left == null)
                    p.left = new Node<T>();
                p = p.left;
            } else { //odd
                if (p.right == null)
                    p.right = new Node<T>();
                p = p.right;
            }
            key >>= 1;  //key = key / 2;
        }
        p.value = val;
        return t;
    }

    public static <T> Optional<T> lookup(Node<T> t, int key) {
        while (t != null && key != 0) {
            t = (0 == (key & 0x1)) ? t.left : t.right;
            key >>= 1;
        }
        return Optional.ofNullable(t == null ? null : t.value);
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
                Optional<T> v = lookup(t, k);
                if (!v.isPresent())
                    throw new RuntimeException(String.format("not found: %d\n", k));
                if (!v.get().equals(m.get(k)))
                    throw new RuntimeException(String.format("lookup key=%d, expect: %s, get: %s\n",
                                                             k, m.get(k).toString(), v.get().toString()));
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

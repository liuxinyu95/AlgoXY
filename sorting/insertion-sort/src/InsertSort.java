/*
 * InsertSort.java
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

public class InsertSort {

    /* insert x[i] to xs[0...i-1] */
    public static int[] sort(int[] xs) {
        for(int i = 1; i < xs.length; ++i) {
            int x = xs[i], j = i - 1;
            while(j >=0 && x < xs[j])
                xs[j+1] = xs[j--];
            xs[j+1] = x;
        }
        return xs;
    }

    /* An equivalent insertion sort, but need more operations. */
    public static int[] isort(int[] xs) {
        int i, j;
        for (i = 1; i < xs.length; ++i)
            for (j = i - 1; j >= 0 && xs[j + 1] < xs[j]; --j)
                swap(xs, j, j + 1);
        return xs;
    }

    static void swap(int[] xs, int i, int j) {
        int tmp = xs[i];
        xs[i] = xs[j];
        xs[j] = tmp;
    }

    /* Using binary search to locate the insert position */
    public static int[] insertSort(int[] xs) {
        for(int i = 1; i < xs.length; ++i) {
            int x = xs[i];
            int p = binarySearch(xs, i, x);
            for (int j = i; j > p; --j)
                xs[j] = xs[j - 1];
            xs[p] = x;
        }
        return xs;
    }

    static int binarySearch(int[] xs, int u, int x) {
        int l = 0;
        while (l < u) {
            int m = l + (u - l) / 2;    // essentially: (l + u) / 2
            if (xs[m] == x) return m;
            else if (xs[m] < x) l = m + 1;
            else u = m;
        }
        return l;
    }

    /* Insertion Sort with linked list setting */

    public static class Node {
        public int key;
        public Node next;
        public Node(int k, Node rest) {
            key = k;
            next = rest;
        }
    }

    public static Node insert(Node list, Node node) {
        Node prev = null;
        Node head = list;
        while (list != null && list.key < node.key) {
            prev = list;
            list = list.next;
        }
        node.next = list;
        if (prev == null)    //node is the new head
            head = node;
        else
            prev.next = node;
        return head;
    }

    public static Node insertionSort(Node list) {
        Node result = null;
        while (list != null) {
            Node node = list;
            list = list.next;
            result = insert(result, node);
        }
        return result;
    }

    /* Insertion sort with index based linked-list setting */

    public static int[] insSort(int[] xs) {
        int[] next = new int[xs.length + 1]; //the last cell point to the head
        Arrays.fill(next, -1);
        for (int i = 0; i < xs.length; ++i)
            ins(xs, next, i);
        return reorder(xs, next);
    }

    static void ins(int[] xs, int[] next, int i) {
        int j = xs.length;  //traverse from head
        while (next[j] != -1 && xs[next[j]] < xs[i])
            j = next[j];
        next[i] = next[j];
        next[j] = i;
    }

    static int[] reorder(int[] xs, int[] next) {
        int[] ys = new int[xs.length];
        for (int j = 0, i = xs.length; next[i] != -1; ++j, i = next[i])
            ys[j] = xs[next[i]];
        return ys;
    }

    /* verification and auxiliary functions */

    static List<Integer> genList(Random gen, int n) {
        List<Integer> xs = IntStream.range(0, n).boxed().collect(Collectors.toList());
        Collections.shuffle(xs);
        return xs.subList(0, gen.nextInt(n));
    }

    static int[] fromList(List<Integer> xs) {
        return xs.stream().mapToInt(Integer::intValue).toArray();
    }

    static List<Integer> toList(int[] xs) {
        return IntStream.of(xs).boxed().collect(Collectors.toList());
    }

    static Node toLinkedList(List<Integer> xs) {
        Node ys = null;
        for (Integer x : xs)
            ys = new Node(x, ys);
        return reverse(ys);
    }

    static Node reverse(Node xs) {
        Node ys = null;
        for (; xs != null; xs = xs.next)
            ys = new Node(xs.key, ys);
        return ys;
    }

    static List<Integer> fromLinkedList(Node list) {
        List<Integer> xs = new ArrayList<>();
        for(; list != null; list = list.next)
            xs.add(list.key);
        return xs;
    }

    static <T> String join(Collection<T> xs) {
        return xs.stream().map(Object::toString).collect(Collectors.joining(", "));
    }

    static <T> void assertEq(List<T> xs, List<T> ys) {
        if (xs.size() != ys.size() || !xs.equals(ys))
            throw new RuntimeException(String.format("[%s] != [%s]", join(xs), join(ys)));
    }

    final static int N = 100;

    public static void main(String[] args) {
        Random gen = new Random();
        for (int i = 0; i < N; ++i) {
            List<Integer> xs = genList(gen, N);
            List<Integer> ys = xs.stream().sorted().collect(Collectors.toList());
            List<Integer> zs = toList(sort(fromList(xs)));
            assertEq(ys, zs);
            zs = toList(isort(fromList(xs)));
            assertEq(ys, zs);
            zs = toList(insertSort(fromList(xs)));
            assertEq(ys, zs);
            zs = fromLinkedList(insertionSort(toLinkedList(xs)));
            assertEq(ys, zs);
            zs = toList(insSort(fromList(xs)));
            assertEq(ys, zs);
        }
        System.out.format("passed %d tests\n", N);
    }
}

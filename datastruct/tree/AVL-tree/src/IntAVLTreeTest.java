/*
 * IntAVLTreeTest.java
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
import static java.lang.Math.*;
import java.lang.Exception;

public class IntAVLTreeTest extends IntAVLTree {
    static boolean isAVL(Node t) {
        if (t == null) return true;
        int delta = height(t.right) - height(t.left);
        return delta == t.delta && isAVL(t.left) && isAVL(t.right) && abs(delta) <= 1;
    }

    static void verifyBST(Node t, List<Integer> xs) {
        if(!toList(t).equals(xs.stream().sorted().collect(Collectors.toList())))
            throw new RuntimeException(String.format("tree %s isn't BST of [%s]",
                toStr(t),
                xs.stream().map(Object::toString).collect(Collectors.joining(", "))));
    }

    static List<Integer> genList(Random gen, int maxLen) {
        List<Integer> xs = IntStream.range(0, maxLen).boxed().collect(Collectors.toList());
        Collections.shuffle(xs);
        return xs.subList(0, gen.nextInt(maxLen));
    }

    public static void testInsert(List<Integer> xs) {
        Node t = fromList(xs);
        verifyBST(t, xs);
        if (!isAVL(t))
            throw new RuntimeException(String.format("build: violate AVL properties: %s", toStr(t)));
    }

    public static Node testDeleteKey(Node t, int k) {
        //System.out.format("del %d from %s\n", k, toStr(t));
        t = del(t, search(t, k));
        //System.out.format("\t==>%s\n", toStr(t));
        if (search(t, k) != null)
            throw new RuntimeException(String.format("Found %d after del.", k));
        if (!isAVL(t))
            throw new RuntimeException(String.format("del: violate AVL properties: %s", toStr(t)));
        return t;
    }

    public static void testDelete(List<Integer> xs) {
        Node t = fromList(xs);
        for (Integer x : xs)
            t = testDeleteKey(t, x);
    }

    final static int N = 100; // can reduce to 20 for debugging.

    public static void main(String[] args) {
        Random gen = new Random();
        for (int i = 0; i < N; ++i) {
            List<Integer> xs = genList(gen, N);
            testInsert(xs);
            testDelete(xs);
        }
        System.out.format("%d test passed.\n", N);
    }
}

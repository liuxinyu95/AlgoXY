/*
 * IntBSTreeTest.java
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

public class IntBSTreeTest extends IntBSTree {
    final static int N = 100;

    static void testBuild(List<Integer> xs) {
        TestUtil.assertEq(toList(fromList(xs)), xs.stream().sorted().collect(Collectors.toList()));
    }

    static void testSearch(List<Integer> xs, int x) {
        final Node tr = search(fromList(xs), x);
        if (tr == null) {
            if (xs.contains(x))
                throw new RuntimeException(String.format("%d exits", x));
        } else {
            if (tr.key != x)
                throw new RuntimeException(String.format("given %d, found %d", x, tr.key));
        }
    }

    static void testMinMax(List<Integer> xs) {
        final Node t = fromList(xs);
        if (t == null) return;
        if (!Collections.min(xs).equals(min(t).key))
            throw new RuntimeException(String.format("min(xs)=%d, min(tree)=%d",
                                                 Collections.min(xs), min(t).key));
        if (!Collections.max(xs).equals(max(t).key))
            throw new RuntimeException(String.format("max(xs)=%d, min(tree)=%d",
                                                 Collections.max(xs), max(t).key));
    }

    static void testSuccPred(List<Integer> xs) {
        if (xs.isEmpty()) return;
        final Node t = fromList(xs);
        List<Integer> ys = xs.stream().sorted().collect(Collectors.toList());
        List<Integer> zs = new ArrayList<>();
        Node p = search(t, ys.get(0));
        while (p != null) {
            zs.add(p.key);
            p = succ(p);
        }
        TestUtil.assertEq(ys, zs);

        p = search(t, ys.get(ys.size() - 1));
        zs.clear();
        while (p != null) {
            zs.add(p.key);
            p = pred(p);
        }
        Collections.reverse(zs);
        TestUtil.assertEq(ys, zs);
    }

    static void testDelete(List<Integer> xs) {
        if (xs.isEmpty()) return;
        Node t = fromList(xs);
        List<Integer> ys = xs.stream().sorted().collect(Collectors.toList());
        for (Integer x : xs) {
            t = delete(t, search(t, x));
            ys.remove(x);
            TestUtil.assertEq(ys, toList(t));
        }
    }

    public static void main(String[] args) {
        final Random gen = new Random();
        for (int i = 0; i < N; ++i) {
            final List<Integer> xs = TestUtil.genList(gen, N);
            testBuild(xs);
            testSearch(xs, gen.nextInt(N));
            testMinMax(xs);
            testSuccPred(xs);
            testDelete(xs);
        }
        System.out.format("passed %d tests.\n", N);
        //traverse(fromList(genList(gen)), System.out::println);
    }
}

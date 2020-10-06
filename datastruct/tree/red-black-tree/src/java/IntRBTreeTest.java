/*
 * IntRBTreeTest.java
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

public class IntRBTreeTest extends IntRBTree {

    Node t1, t2;

    private static Color colorOf(char c) {
        switch (c) {
        case 'R':
            return Color.RED;
        case 'B':
            return Color.BLACK;
        default:
            return Color.DOUBLY_BLACK;
        }
    }

    private static Node nodeOf(int x, char c) {
        return new Node(x, colorOf(c));
    }

    private static Node tr(Node l, int x, char c, Node r) {
        Node t = nodeOf(x, c);
        t.setChildren(l, r);
        return t;
    }

    public IntRBTreeTest() {
        // t1 = ((1:B, 2:R, (3:R, 4:B, .)), 5:B, (6:B, 7:R, (8:R, 9:B, .)))
        t1 = tr(tr(nodeOf(1, 'B'), 2, 'R', tr(nodeOf(3, 'R'), 4, 'B', null)),
                   5, 'B',
                   tr(nodeOf(6, 'B'), 7, 'R', tr(nodeOf(8, 'R'), 9, 'B', null)));
        System.out.format("t1 1..9\n%s\n", toStr(t1));

        /*
         * t2 as figure 13.4 in CLRS
         * (((. 1:B .) 2:R ((. 5:R .) 7:B (. 8:R .))) 11:B (. 14:B (. 15:R .)))
         */
        t2 = tr(tr(nodeOf(1, 'B'), 2, 'R', tr(nodeOf(5, 'R'), 7, 'B', nodeOf(8, 'R'))),
                11, 'B',
                tr(null, 14, 'B', nodeOf(15, 'R')));
        System.out.format("t2, CLRS fig 13.4\n%s\n", toStr(t2));

    }

    // verifiy red-black tree properties
    static boolean isRBTree(Node t) {
        if (t == null) return true;
        if (t.color != Color.BLACK) {
            System.out.println("root is not black.");
            return false;
        }
        if (hasAdjacentRed(t)) {
            System.out.println("has adjacent red nodes");
            return false;
        }
        if (numOfBlacks(t) < 0) {
            System.out.println("different number of black nodes");
            return false;
        }
        return true;
    }

    static boolean hasAdjacentRed(Node t) {
        if (t == null) return false;
        if (isRed(t) && (isRed(t.left) || isRed(t.right))) {
            System.out.format("adjacent red at %d", t.key);
            return true;
        }
        return hasAdjacentRed(t.left) || hasAdjacentRed(t.right);
    }

    static int numOfBlacks(Node t) {
        if (t == null) return 1;
        int a = numOfBlacks(t.left);
        int b = numOfBlacks(t.right);
        if (a != b) {
            System.out.format("Node %d has different black descendants: l=%d, r=%d\n", t.key, a, b);
            return -10000;
        }
        return a + (isBlack(t) ? 1 : 0);
    }

    static void assertEq(Node a, Node b) {
        String s1 = toStr(a);
        String s2 = toStr(b);
        if (!s1.equals(s2))
            throw new RuntimeException(String.format("%s != %s", s1, s2));
    }

    static void assertRBTree(Node t) {
        if (!isRBTree(t))
            throw new RuntimeException("Violate Red-black tree properties.");
    }

    public void testRotate() {
        Node t = clone(t1);
        Node x = t.right;  // 7:R
        t = rotateLeft(t, x); // (6 7 (8 9 .)) ==> ((6 7 8) 9 .)
        System.out.format("rotate left at 7:R\n%s\n", toStr(t));
        t = rotateRight(t, t.right); // rotate back
        System.out.format("rotate right back:\n%s\n", toStr(t));
        assertEq(t, t1);

        t = rotateLeft(t, t);  // (2 5 (6 7 9)) ==> ((2 5 6) 7 9)
        System.out.format("rotate left at root:\n%s\n", toStr(t));
        t = rotateRight(t, t); // rotate back
        System.out.format("rotate right back:\n%s\n", toStr(t));
        assertEq(t, t1);
    }

    public void testInsert() {
        Node t = clone(t2);
        t = insert(t, 4);
        System.out.format("t2: after insert 4:\n%s\n", toStr(t));
        assertRBTree(t);

        t = fromList(IntStream.of(5, 2, 7, 1, 4, 6, 9, 3, 8).boxed().collect(Collectors.toList()));
        System.out.format("list->tree, create t1 by insertion\n%s\n", toStr(t));
        assertEq(t, t1);
        assertRBTree(t);
    }

    public void testDeleteKey(Node root, int k) {
        Node t = clone(root);
        System.out.format("del %d from %s\n", k, toStr(t));
        t = del(t, search(t, k));
        System.out.format("\t==>%s\n", toStr(t));
        if (search(t, k) != null)
            throw new RuntimeException(String.format("Found %d after del.", k));
        assertRBTree(t);
    }

    public void testDelete() {
        for (int i = 1; i < 10; ++i)
            testDeleteKey(t1, i);
        testDeleteKey(t1, 11); // del a non-exist key
        Node t = new Node(1, Color.BLACK);   // delete from a leaf
        testDeleteKey(t, 1);
    }

    public void run() {
        testRotate();
        testInsert();
        testDelete();
    }

    public static void main(String[] args) {
        (new IntRBTreeTest()).run();
    }
}

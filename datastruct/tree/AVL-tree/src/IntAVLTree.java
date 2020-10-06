/*
 * IntAVLTree.java
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
import static java.lang.Math.*;

/* AVL tree, with integral key for illustration purpose. */

public class IntAVLTree {
    public static class Node {
        public int key;
        public int delta;  // balance factor
        public Node left;
        public Node right;
        public Node parent;

        public Node(int k) { key = k; }

        void setLeft(Node x) {
            left = x;
            if (x != null) x.parent = this;
        }

        void setRight(Node x) {
            right = x;
            if (x != null) x.parent = this;
        }

        void setChildren(Node x, Node y) {
            setLeft(x);
            setRight(y);
        }

        Node replaceWith(Node y) {
            return replace(parent, this, y);
        }
    }

    public static boolean isLeaf(Node x) {
        return x != null && x.left == null && x.right == null;
    }

    // change from: parent --> x to parent --> y
    public static Node replace(Node parent, Node x, Node y) {
        if (parent == null) {
            if (y != null) y.parent = null;
        } else if (parent.left == x) {
            parent.setLeft(y);
        } else {
            parent.setRight(y);
        }
        if (x != null) x.parent = null;
        return y;
    }

    // Rotation. It doesn't change the delta

    // rotate left (a, x, (b, y, c)) ==> ((a, x, b), y, c)
    public static Node rotateLeft(Node t, Node x) {
        Node parent = x.parent;
        Node y = x.right;
        Node a = x.left;
        Node b = y.left;
        Node c = y.right;
        x.replaceWith(y);
        x.setChildren(a, b);
        y.setChildren(x, c);
        if (parent == null) t = y;
        return t;
    }

    // rotate right: (a, x, (b, y, c) <== ((a, x, b), y, c)
    public static Node rotateRight(Node t, Node y) {
        Node parent = y.parent;
        Node x = y.left;
        Node a = x.left;
        Node b = x.right;
        Node c = y.right;
        y.replaceWith(x);
        y.setChildren(b, c);
        x.setChildren(a, y);
        if (parent == null) t = x;
        return t;
    }

    // top-down insert, returns the new root
    public static Node insert(Node t, int key) {
        Node root = t;
        Node x = new Node(key);
        Node parent = null;
        while (t != null) {
            parent = t;
            t = key < t.key ? t.left : t.right;
        }
        if (parent == null) { // insert key to the empty tree
            root = x;
        } else if (key < parent.key) {
            parent.setLeft(x);
        } else {
            parent.setRight(x);
        }
        return insertFix(root, parent, x);
    }

    /*
     * Bottom-up update delta and fix
     *   t: tree root;
     *   x: the sub-tree that the height changes.
     *
     * Denote d = delta(x), d' = delta(x'),
     *   where x' is the new sub tree after insertion.
     *
     * case 1: |d| == 0, |d'| == 1,
     *    It means height increase, go on bottom-up updating.
     *
     * case 2: |d| == 1, |d'| == 0,
     *    program terminate as height doesn't change.
     *
     * case 3: |d| == 1, |d'| == 2, AVL violation,
     *    we need fixing by rotation.
     */
    private static Node insertFix(Node t, Node parent, Node x) {
        while (parent != null) {
            int d1 = parent.delta;
            int d2 = d1 + (x == parent.left ? -1 : 1);
            parent.delta = d2;
            Node p = parent;
            Node l = parent.left;
            Node r = parent.right;
            if (abs(d1) == 1 && abs(d2) == 0) {
                return t;
            } else if (abs(d1) == 0 && abs(d2) == 1) {
                x = parent;
                parent = x.parent;
            } else if (abs(d1) == 1 && abs(d2) == 2) {
                if (d2 == 2) {
                    if (r.delta == 1) { // right-right case
                        p.delta = 0;
                        r.delta = 0;
                        t = rotateLeft(t, p);
                    } else if (r.delta == -1) { // right-left case
                        int dy = r.left.delta;
                        p.delta = dy == 1 ? -1 : 0;
                        r.left.delta = 0;
                        r.delta = dy == -1 ? 1 : 0;
                        t = rotateRight(t, r);
                        t = rotateLeft(t, p);
                    }
                } else if (d2 == -2) {
                    if (l.delta == -1) { // left-left case
                        p.delta = 0;
                        l.delta = 0;
                        t = rotateRight(t, p);
                    } else if (l.delta == 1) { // left-right case
                        int dy = l.right.delta;
                        l.delta = dy == 1 ? -1 : 0;
                        l.right.delta = 0;
                        p.delta = dy == -1 ? 1 : 0;
                        t = rotateLeft(t, l);
                        t = rotateRight(t, p);
                    }
                }
                break;
            } else {
                throw new RuntimeException(String.format(
                    "shouldn't be here, d1 = %d, d2 = %d", d1, d2));
            }
        }
        return t;
    }

    public static Node del(Node t, Node x) {
        if (x == null) return t;
        Node y, parent = x.parent;
        if (x.left == null) {
            y = x.replaceWith(x.right);
        } else if (x.right == null) {
            y = x.replaceWith(x.left);
        } else {
            y = min(x.right);
            x.key = y.key;
            parent = y.parent;
            y = y.replaceWith(y.right);
        }
        return deleteFix(t, parent, y);
    }

    /*
     * Bottom-up update delta and fix
     *   t: tree root;
     *   x: the sub-tree that the height changes.
     *
     * Denote d = delta(x), d' = delta(x'),
     *   where x' is the new sub tree after deletion.
     *
     * case 1: |d| == 0, |d'| == 1,
     *    Program terminate as height doesn't change.
     *
     * case 2: |d| == 1, |d'| == 0,
     *    For delete, it means height decrease, go on bottom-up updating.
     *
     * case 3: |d| == 1, |d'| == 2, AVL violation,
     *    we need fixing by rotation.
     */
    private static Node deleteFix(Node t, Node parent, Node x) {
        while (parent != null) {
            int d1 = parent.delta;
            int d2 = d1 + (x == parent.left ? 1 : -1);
            if (isLeaf(parent))
                d2 = 0;
            parent.delta = d2;
            Node p = parent;
            Node l = parent.left;
            Node r = parent.right;
            if (abs(d1) == 1 && abs(d2) == 0) {
                x = parent;
                parent = x.parent;
            } else if (abs(d1) == 0 && abs(d2) == 1) {
                return t;
            } else if (abs(d1) == 1 && abs(d2) == 2) {
                if (d2 == 2) {
                    if (r.delta == 1) { // right-right case
                        p.delta = 0;
                        r.delta = 0;
                        parent = r;
                        t = rotateLeft(t, p);
                    } else if (r.delta == -1) { // right-left case
                        int dy = r.left.delta;
                        p.delta = dy == 1 ? -1 : 0;
                        r.left.delta = 0;
                        r.delta = dy == -1 ? 1 : 0;
                        parent = r.left;
                        t = rotateRight(t, r);
                        t = rotateLeft(t, p);
                    } else { // del specific right-right case
                        p.delta = 1;
                        r.delta--;
                        t = rotateLeft(t, p);
                        break; // no further height change
                    }
                } else if (d2 == -2) {
                    if (l.delta == -1) { // left-left case
                        p.delta = 0;
                        l.delta = 0;
                        parent = l;
                        t = rotateRight(t, p);
                    } else if (l.delta == 1) { // left-right case
                        int dy = l.right.delta;
                        l.delta = dy == 1 ? -1 : 0;
                        l.right.delta = 0;
                        p.delta = dy == -1 ? 1 : 0;
                        parent = l.right;
                        t = rotateLeft(t, l);
                        t = rotateRight(t, p);
                    } else { // del specific left-left case
                        p.delta = -1;
                        l.delta++;
                        t = rotateRight(t, p);
                        break; // no further height change
                    }
                }
                // the 4 rebalance cases cause height decrease, need bottom-up update
                x = parent;
                parent = x.parent;
            } else {
                throw new RuntimeException(String.format(
                    "shouldn't be here, d1 = %d, d2 = %d", d1, d2));
            }
        }
        if (parent == null) // delete the root
            return x;
        return t;
    }

    public static Node min(Node t) {
        while (t != null && t.left != null) t = t.left;
        return t;
    }

    public static Node search(Node t, int x) {
        while (t != null && t.key != x)
            t = x < t.key ? t.left : t.right;
        return t;
    }

    // Auxiliary

    public static int height(Node t) {
        return t == null ? 0 : (1 + max(height(t.left), height(t.right)));
    }

    public static List<Integer> toList(Node t) {
        if (t == null) return new ArrayList<Integer>();
        List<Integer> xs = toList(t.left);
        xs.add(t.key);
        xs.addAll(toList(t.right));
        return xs;
    }

    public static Node fromList(Collection<Integer> xs) {
        Node t = null;
        for (Integer x : xs)
            t = insert(t, x);
        return t;
    }

    public static String toStr(Node t) {
        if (t == null) return ".";
        return String.format("(%s %d:%d %s)", toStr(t.left), t.key, t.delta,
                             toStr(t.right));
    }
}

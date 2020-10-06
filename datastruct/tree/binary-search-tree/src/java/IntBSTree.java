/*
 * IntBSTree.java
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
import java.util.function.*;

/* For illustration purpose, integer based BST needn't deal with comparable interface */
public class IntBSTree {

    public static class Node {
        public int key;
        public Node left;
        public Node right;
        public Node parent; //optional, mainly used for succ/pred.

        public Node(int x) {
            key = x;
        }
    }

    // in order traverse
    public static void traverse(Node t, Consumer<Integer> f) {
        if (t != null) {
            traverse(t.left, f);
            f.accept(t.key);
            traverse(t.right, f);
        }
    }

    public static Node insert(Node tr, int key) {
        Node root = tr, x = new Node(key), parent = null;
        while (tr != null) {
            parent = tr;
            tr = (key < tr.key) ? tr.left : tr.right;
        }
        x.parent = parent;
        if (parent == null) // tree is empty
            return x;
        else if (key < parent.key)
            parent.left = x;
        else
            parent.right = x;
        return root;
    }

    public static Node search(Node tr, int x) {
        while (tr != null && tr.key != x)
            tr = x < tr.key ? tr.left : tr.right;
        return tr;
    }

    public static Node min(Node t) {
        while (t != null && t.left != null)
            t = t.left;
        return t;
    }

    public static Node max(Node t) {
        while (t != null && t.right != null)
            t = t.right;
        return t;
    }

    // assume the input x isn't null when find succ/pred

    public static Node succ(Node x) {
        if (x.right != null)
            return min(x.right);
        Node p = x.parent;
        while (p != null && p.left != x) {
            x = p;
            p = p.parent;
        }
        return p;
    }

    public static Node pred(Node x) {
        if (x.left != null)
            return max(x.left);
        Node p = x.parent;
        while (p != null && p.right != x) {
            x = p;
            p = p.parent;
        }
        return p;
    }

    static void cleanup(Node x) {
        if (x != null)
            x.left = x.right = x.parent = null;
    }

    public static Node delete(Node t, Node x) {
        if (x == null) return t;
        Node root = t;
        Node node = x;
        Node parent = x.parent;
        if (x.left == null)
            x = x.right;
        else if (x.right == null)
            x = x.left;
        else {
            Node y = min(x.right);
            x.key = y.key;
            if (y.parent != x)
                y.parent.left = y.right;
            else
                x.right = y.right;
            if (y.right != null)
                y.right.parent = y.parent;
            cleanup(y);
            return root;
        }
        if (x != null)
            x.parent = parent;
        if (parent == null)
            root = x;
        else {
            if (parent.left == node)
                parent.left = x;
            else
                parent.right = x;
        }
        cleanup(node);
        return root;
    }

    // Auxiliary

    public static Node fromList(Collection<Integer> xs) {
        Node t = null;
        for (Integer x : xs)
            t = insert(t, x);
        return t;
    }

    public static List<Integer> toList(Node t) {
        if (t == null) return new ArrayList<Integer>();
        List<Integer> xs = toList(t.left);
        xs.add(t.key);
        xs.addAll(toList(t.right));
        return xs;
    }
}

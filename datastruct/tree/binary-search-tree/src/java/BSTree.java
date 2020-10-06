/*
 * BSTree.java
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

public class BSTree {

    public static class Node<T> {
        public T key;
        public Node<T> left;
        public Node<T> right;
        public Node<T> parent; //optional, mainly used for succ/pred.

        public Node(T x) {
            key = x;
        }
    }

    // in order traverse
    public static <T> void traverse(Node<T> t, Consumer<T> f) {
        if (t != null) {
            traverse(t.left, f);
            f.accept(t.key);
            traverse(t.right, f);
        }
    }

    public static <T extends Comparable<? super T>> Node<T> insert(Node<T> tr, T key) {
        Node<T> root = tr, x = new Node<>(key), parent = null;
        while (tr != null) {
            parent = tr;
            tr = (key.compareTo(tr.key) < 0) ? tr.left : tr.right;
        }
        x.parent = parent;
        if (parent == null) // tree is empty
            return x;
        else if (key.compareTo(parent.key) < 0)
            parent.left = x;
        else
            parent.right = x;
        return root;
    }

    public static <T extends Comparable<? super T>> Node<T> search(Node<T> tr, T x) {
        while (tr != null && !tr.key.equals(x))
            tr = (x.compareTo(tr.key) < 0) ? tr.left : tr.right;
        return tr;
    }

    public static <T> Node<T> min(Node<T> tr) {
        while (tr != null && tr.left != null)
            tr = tr.left;
        return tr;
    }

    public static <T> Node<T> max(Node<T> t) {
        while (t != null && t.right != null)
            t = t.right;
        return t;
    }

    public static <T> Node<T> succ(Node<T> x) {
        if (x.right != null)
            return min(x.right);
        Node<T> p = x.parent;
        while (p != null && p.left != x) {
            x = p;
            p = p.parent;
        }
        return p;
    }

    public static <T> Node<T> pred(Node<T> x) {
        if (x.left != null)
            return max(x.left);
        Node<T> p = x.parent;
        while (p != null && p.right != x) {
            x = p;
            p = p.parent;
        }
        return p;
    }


    // auxiliary utilities
    public static <T extends Comparable<? super T>> Node<T> fromList(Collection<T> xs) {
        Node<T> t = null;
        for (T x : xs)
            t = insert(t, x);
        return t;
    }

    public static <T> List<T> toList(Node<T> t) {
        if (t == null) return new ArrayList<T>();
        final List<T> xs = toList(t.left);
        xs.add(t.key);
        xs.addAll(toList(t.right));
        return xs;
    }
}

/*
 * LinkTreeNodes.java
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

public class LinkTreeNodes {
    static class Node<T> {
        T key;
        Node<T> left;
        Node<T> right;

        public Node(T k, Node<T> l, Node<T> r) {
            key = k; left = l; right = r;
        }
    }

    static <T> Node<T> branch(T k) { return branch(k, null, null); }

    static <T> Node<T> branch(T k, Node<T> l, Node<T> r) {
        return new Node<T>(k, l, r);
    }

    static <T> LinkedList<T> singleton(T x) {
        return new LinkedList<T>() {{ add(x); }};
    }

    /* links children in same level. returns list of list. */
    public static <T> List<List<Node<T>>> link(Node<T> t) {
        Queue<List<Node<T>>> q = singleton(singleton(t));
        List<List<Node<T>>> r = singleton(singleton(t));
        while (!q.isEmpty()) {
            List<Node<T>> list = new LinkedList<Node<T>>();
            for(Node<T> node : q.poll()) {
                if (node.left != null)
                    list.add(node.left);
                if (node.right != null)
                    list.add(node.right);
            }
            if (!list.isEmpty()) {
                q.add(list);
                r.add(list);
            }
        }
        return r;
    }

    static <T> void print(List<List<Node<T>>> xss) {
        System.out.println("lists of nodes");
        for (List<Node<T>> xs : xss) {
            for (Node<T> x : xs) {
                System.out.print(x.key);
                System.out.print("-->");
            }
            System.out.println("");
        }
    }

    public static void main(String[] args) {
        Node<Integer> t = branch(5, branch(3, branch(1), branch(4)), branch(8, branch(6), null));
        print(link(t));
    }
}

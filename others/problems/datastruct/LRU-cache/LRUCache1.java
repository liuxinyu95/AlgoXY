/*
 * LRUCache1.java
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
import java.util.Map;
import java.util.HashMap;

/*
 * LRU is Least Recently Used cache.
 * When it reaches the capacity limit, the cache drops the least recently
 * used record.
 */

/* LC OJ ver. */
public class LRUCache1 {
    static class Node {
        Node prev;
        Node next;
        int key;
        int value;
        Node(int k, int v) { key = k; value = v; }
    }

    Node head;
    Node tail;
    Map<Integer, Node> map = new HashMap<Integer, Node>();
    int size = 0;
    final private int capacity;

    static Node link(Node x, Node y) {
        if (x != null) x.next = y;
        if (y != null) y.prev = x;
        return x;
    }

    Node remove(Node x) {
        if (x == head) head = x.next;
        if (x == tail) tail = x.prev;
        link(x.prev, x.next);
        x.prev = x.next = null;
        return x;
    }

    void pop() {
        if (tail != null) map.remove(remove(tail).key);
    }

    void insert(Node x) {
        head = link(x, head);
        if (tail == null) tail = x;
    }

    public LRUCache1(int capacity) {
        this.capacity = capacity;
    }

    public int get(int key) {
        final Node x = map.get(key);
        if (x != null) {
            insert(remove(x)); /* move to front */
            return x.value;
        }
        return -1;
    }

    public void set(int key, int value) {
        final Node x = map.get(key);
        if (x != null) {
            x.value = value;
            insert(remove(x));
        } else {
            if (size == capacity) pop();
            map.put(key, new Node(key, value));
            insert(map.get(key));
            size++;
        }
    }

    public void print() {
        System.out.print("[");
        for (Node x = head; x != null; x = x.next) {
            System.out.printf("(%d:%d)-->", x.key, x.value);
        }
        System.out.println("]");

    }

    public static void main(String[] args) {
        LRUCache1 cache = new LRUCache1(3);
        cache.set(1, 1);
        cache.set(2, 2);
        cache.set(3, 3);
        System.out.println(String.format("1->%d", cache.get(1)));
        System.out.println(String.format("1->%d", cache.get(1)));
        cache.print();
        System.out.println(String.format("2->%d", cache.get(2)));
    }
}

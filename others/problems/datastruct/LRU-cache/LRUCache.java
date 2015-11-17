import java.util.Map;
import java.util.HashMap;

/*
 * LRU is Least Recently Used cache.
 * When it reaches the capacity limit, the cache drops the least recently
 * used record.
 */

/* HashMap + Doubly linked-list */
public class LRUCache<K, V> {

    /* Doubly linked-list */
    static class Node<K, V> {
        Node<K, V> prev;
        Node<K, V> next;
        K key;             /* For removing the mapping */
        V value;

        Node(K k, V v) { key = k; value = v; }
    }

    private Node<K, V> head;
    private Node<K, V> tail;
    private Map<K, Node<K, V>> map = new HashMap<K, Node<K, V>>();
    private int size = 0;
    final private int capacity;

    static <K, V> Node<K, V> link(Node<K, V> x, Node<K, V> y) {
        if (x != null)
            x.next = y;
        if (y != null)
            y.prev = x;
        return x;
    }

    /* remove node x from the list */
    private Node<K, V> remove(Node<K, V> x) {
        if (x == head) head = x.next;
        if (x == tail) tail = x.prev;
        link(x.prev, x.next);
        x.prev = x.next = null;
        return x;
    }

    /* remove tail. */
    private void pop() {
        if (tail != null) {
            map.remove(remove(tail).key);
            size--;
        }
    }

    /* insert node x before head. */
    void insert(Node<K, V> x) {
        head = link(x, head);
        if (tail == null)
            tail = x;
    }

    public LRUCache(int capacity) {
        this.capacity = capacity;
    }

    public V get(K key) {
        final Node<K, V> x = map.get(key);
        if (x != null) {
            insert(remove(x));  /* move to front */
            return x.value;
        }
        return null;
    }

    public void set(K key, V value) {
        final Node<K, V> x = map.get(key);
        if (x != null) {
            x.value = value;
            insert(remove(x));
        }
        else {
            if (size == capacity) pop();
            map.put(key, new Node<K, V>(key, value));
            insert(map.get(key));
            size++;
        }
    }

    public static void main(String[] args) {
        LRUCache cache = new LRUCache<Integer, String>(3);
        cache.set(1, "one");
        cache.set(2, "two");
        cache.set(3, "three");
        System.out.println(String.format("1->%s", cache.get(1)));
        cache.set(4, "four");
        System.out.println(String.format("1->%s", cache.get(1)));
        System.out.println(String.format("2->%s", cache.get(2)));
    }
}

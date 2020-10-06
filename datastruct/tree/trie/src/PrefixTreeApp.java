import java.util.*;
import java.util.stream.*;
import java.lang.Math;
import java.lang.Exception;

public class PrefixTreeApp {
    // returns up to n candidates start with given prefix
    public static <T> List<Map.Entry<String, T>> lookup(PrefixTree.Node<T> t,
                                                        String key, int n) {
        if (t == null)
            return Collections.emptyList();
        String prefix = "";
        boolean match;
        do {
            match = false;
            for (Map.Entry<String, PrefixTree.Node<T>> entry : t.subTrees.entrySet()) {
                String k = entry.getKey();
                PrefixTree.Node<T> tr = entry.getValue();
                if (k.startsWith(key)) { // key is prefix of k
                    return expand(prefix + k, tr, n);
                }
                if (key.startsWith(k)) {
                    match = true;
                    key = key.substring(k.length());
                    t = tr;
                    prefix = prefix + k;
                    break;
                }
            }
        } while (match);
        return Collections.emptyList();
    }

    static <T> List<Map.Entry<String, T>> expand(String prefix, PrefixTree.Node<T> t, int n) {
        List<Map.Entry<String, T>> res = new ArrayList<>();
        Queue<Map.Entry<String, PrefixTree.Node<T> >> q = new LinkedList<>();
        q.offer(entryOf(prefix, t));
        while(res.size() < n && !q.isEmpty()) {
            Map.Entry<String, PrefixTree.Node<T>> entry = q.poll();
            String s = entry.getKey();
            PrefixTree.Node<T> tr = entry.getValue();
            if (tr.value.isPresent()) {
                res.add(entryOf(s, tr.value.get()));
            }
            for (Map.Entry<String, PrefixTree.Node<T>> e :
                     new TreeMap<>(tr.subTrees).entrySet()) {
                q.offer(entryOf(s + e.getKey(), e.getValue()));
            }
        }
        return res;
    }

    static <K, V> Map.Entry<K, V> entryOf(K key, V val) {
        return new AbstractMap.SimpleImmutableEntry<K, V>(key, val);
    }

    // T9 map
    static final Map<Character, String> MAP_T9 = new HashMap<Character, String>(){{
            put('1', ",."); put('2', "abc"); put('3', "def");
            put('4', "ghi"); put('5', "jkl"); put('6', "mno");
            put('7', "pqrs"); put('8', "tuv"); put('9', "wxyz");
        }};

    // T9 reverse map
    static final Map<Character, Character> RMAP_T9 = new HashMap<Character, Character>(){{
            for (Character d : MAP_T9.keySet()) {
                String cs = MAP_T9.get(d);
                for (int i = 0; i < cs.length(); ++i)
                    put(cs.charAt(i), d);
            }
        }};

    /*
     * The T9 reverse map can be built with stream, but it's hard to read
     *
     *    static final Map<Character, Character> RMAP_T9 = MAP_T9.entrySet().stream()
     *        .flatMap(e -> e.getValue().chars().mapToObj(i -> (char)i)
     *                 .map(c -> entryOf(c, e.getKey())))
     *        .collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue()));
     */

    public static String digits(String w) {
        StringBuilder d = new StringBuilder();
        for(int i = 0; i < w.length(); ++i)
            d.append(RMAP_T9.get(w.charAt(i)));
        return d.toString();
    }

    static class Tuple<T> {
        String prefix;
        String key;
        PrefixTree.Node<T> tree;
        Tuple(String p, String k, PrefixTree.Node<T> t) {
            prefix = p; key = k; tree = t;
        }

        public static <T> Tuple<T> of(String prefix, String key,
                                      PrefixTree.Node<T> tree) {
            return new Tuple<T>(prefix, key, tree);
        }
    }

    static String limit(int n, String s) {
        return s.substring(0, Math.min(n, s.length()));
    }

    public static <T> List<String> lookupT9(PrefixTree.Node<T> t, String key) {
        List<String> res = new ArrayList<>();
        if (t == null || key.isEmpty())
            return res;
        Queue<Tuple<T>> q = new LinkedList<>();
        q.offer(Tuple.of("", key, t));
        while (!q.isEmpty()) {
            Tuple<T> elem = q.poll();
            for (Map.Entry<String, PrefixTree.Node<T>> e :
                     elem.tree.subTrees.entrySet()) {
                String k = e.getKey();
                String ds = digits(k);
                if (ds.startsWith(elem.key)) {
                    res.add(limit(key.length(), elem.prefix + k));
                } else if (elem.key.startsWith(ds)) {
                    q.offer(Tuple.of(elem.prefix + k,
                                     elem.key.substring(k.length()),
                                     e.getValue()));
                }
            }
        }
        return res;
    }

    public static class Test {
        final static String[] testKeys =
            new String[]{"a", "an", "another", "abandon", "about", "adam", "boy", "body", "zoo"};
        final static String[] testVals =
            new String[]{"the first letter of English",
                         "used instead of 'a' when the following word begins witha vowel sound",
                         "one more person or thing or an extra amount",
                         "to leave a place, thing or person forever",
                         "on the subject of; connected with",
                         "a character in the Bible who was the first man made by God",
                         "a male child or, more generally, a male of any age",
                         "the whole physical structure that forms a person or animal",
                         "an area in which animals, especially wild animals, are kept so that people can go and look at them, or study them"};

        static void testEdict() {
            PrefixTree.Node<String> t = null;
            Map<String, String> m = new HashMap<>();
            int n = Math.min(testKeys.length, testVals.length);
            for (int i = 0; i < n; ++i) {
                t = PrefixTree.insert(t, testKeys[i], testVals[i]);
                m.put(testKeys[i], testVals[i]);
            }
            verifyLookup(m, t, "a", 5);
            verifyLookup(m, t, "a", 6);
            verifyLookup(m, t, "a", 7);
            verifyLookup(m, t, "ab", 2);
            verifyLookup(m, t, "ab", 5);
            verifyLookup(m, t, "b", 2);
            verifyLookup(m, t, "bo", 5);
            verifyLookup(m, t, "z", 3);
        }

        static void verifyLookup(Map<String, String> m, PrefixTree.Node<String> t, String key, int n) {
            System.out.format("lookup %s with limit: %d\n", key, n);
            SortedMap<String, String> m1 = new TreeMap<>();
            for (Map.Entry<String, String> e : lookup(t, key, n)) {
                m1.put(e.getKey(), e.getValue());
            }
            SortedMap<String, String> m2 =
                take(n, toSortedMap(m.entrySet().stream()
                                    .filter(e -> e.getKey().startsWith(key))));
            if (!m2.equals(m1))
                throw new RuntimeException("\n" + m1.toString() + "\n!=\n" + m2.toString());

            System.out.println("result:\n" + m1.toString());
        }

        static <T> SortedMap<String, T> take(int n, SortedMap<String, T> m) {
            return toSortedMap(m.entrySet().stream().limit(n));
        }

        static <K, V> SortedMap<K, V> toSortedMap(Stream<Map.Entry<K, V>> s) {
            return new TreeMap<>(s.collect(Collectors.toMap(e -> e.getKey(), e ->e.getValue())));
        }

        public static void testT9() {
            //System.out.println("T9 map: " + MAP_T9);
            //System.out.println("reverse T9 map: " + RMAP_T9);
            final String txt = "home good gone hood a another an";
            final String[] words = txt.split("\\W+");
            PrefixTree.Node<Integer> t = PrefixTree.fromString(txt);
            for (String testDigits : new String[]{"4663", "22", "2668437"}) {
                for (int i = 1; i <= testDigits.length(); ++i) {
                    String ds = limit(i, testDigits);
                    SortedSet<String> as = new TreeSet<>(lookupT9(t, ds));
                    SortedSet<String> bs = new TreeSet<>();
                    for (String w : words) {
                        String candidate = limit(i, w);
                        if (digits(candidate).equals(ds))
                            bs.add(candidate);
                    }
                    //System.out.println("T9 look up: " + as);
                    //System.out.println("Brute force:" + bs);
                    if (!as.equals(bs))
                        throw new RuntimeException("T9 look up " + as + "\n!=\n" +
                                                   "Brute force" + bs + "\n");
                }
            }
            System.out.println("T9 verified");
        }

        public static void test() {
            testEdict();
            testT9();
        }
    }
}

import java.util.Map;

/*
 * Problem: Given multiple product discount plans,
 *   for e.g. sell products A, B, C together at price X
 * For a wish list of product type [M, N, ...] find the cheapest purchase plan
 *
 * For example, with the below plans:
 */

public class Purchase {
    private static final Map<String, Integer> INPUT1 =
        Map.of("AB",  100,
               "BCD", 150,
               "AD",  125,
               "CD",  135);

    /*
     * Below are the cheapest purchase plan:
     * lowest cost of "BAD"  # ["AB", "AD"]  ==> 225
     * lowest cost of "BAC"  # ["AB", "CD"]  ==> 235
     * lowest cost of "BCD"  # ["BCD"] ==> 150
     *
     * Here is another example. We use 0-9A-Z to enumerate products:
     */

    private static final Map<String, Integer> INPUT2 =
        Map.of("816309", 11,
               "7824", 28,
               "487i620", 47,
               "649", 57,
               "407396812", 57,
               "986750123", 64,
               "9642", 86,
               "16480579", 107,
               "9648350", 111,
               "8937514", 128);

    /*
     * the optimal purchase for product list "704938521" is
     * ["986750123", "7824"] ==> 28 + 64 = 92
     */

    /*
     * Dynamic Programming solution
     * accepts discount plan, returns the DP table {cost: [products]}
     */
    private Map<Integer, Set<String>> dp(Map<String, Integer> plans) {
        Map<Integer, Set<String> tab = new HashMap<>() {{
                put(0, new HasSet<String>());
            }};    //DP table
        for (String pkg : plans.keySet()) {
            for (Integer cost : new HashSet<>(tab.keySet())) {
                Set<String> pkgs = new HashSet<>(tab.get(cost));
                cost += plans.get(pkg);
                if (!(tab.contains(cost) &&
                      union(pkgs).containsAll(strToSet(pkg)))) {
                    pkgs.add(pkg);
                    tab.put(cost, pkgs);
                }
            }
        }
        return tab;
    }

    public Set<String> lowest(Set<Char> prods, Map<Integer, Set<String>> tab) {
        return tab.keySet().stream().sorted()
            .filter(p -> union(tab.get(p)).containsAll(prods))
            .findFirst().orElse(Collections.EMPTY_SET);
    }

    private static Set<Char> union(Set<String> set) {
        Set<Char> s = new HashSet<>();
        for (String str : set) {
            s.addAll(strToSet(str));
        }
        return s;
    }

    private static strToSet(String s) {
        return s.chars().mapToObj(c -> (char) c)
            .collect(Collectors.toSet());
    }

    // Brute force solution for verification purpose

    // DFS based Brute force solution

    public static void main(String[] args) {
    }
}

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;
import java.util.stream.Collectors;

/*
 * Problem: Given multiple product discount plans,
 *   for e.g. sell products A, B, C together at price X
 * For a wish list of product type [M, N, ...] find the cheapest purchase plan
 *
 * For example, with the below plans:
 */

public class Purchase {
    private static final Map<String, Integer> TEST_PLAN1 =
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

    private static final Map<String, Integer> TEST_PLAN2 =
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

    private final static Set<String> EMPTY = Collections.EMPTY_SET;

    private static Set<String> empty() { return new HashSet<>(); }

    /*
     * Dynamic Programming solution
     * accepts discount plan, returns the DP table {cost: [products]}
     */
    private static Map<Integer, Set<String>> dp(Map<String, Integer> plans) {
        Map<Integer, Set<String>> tab = new HashMap<>() {{
                put(0, EMPTY);
            }};
        for (String pkg : plans.keySet()) {
            for (Integer cost : new HashSet<>(tab.keySet())) {
                Set<String> pkgs = new HashSet<>(tab.get(cost));
                cost += plans.get(pkg);
                if (!(tab.containsKey(cost) &&
                      union(pkgs).containsAll(strToSet(pkg)))) {
                    pkgs.add(pkg);
                    tab.put(cost, pkgs);
                }
            }
        }
        return tab;
    }

    public static Set<String> lowest(Set<Character> prods,
                                     Map<Integer, Set<String>> tab) {
        return tab.keySet().stream().sorted()
            .map(p -> tab.get(p))
            .filter(s -> union(s).containsAll(prods))
            .findFirst().orElse(EMPTY);
    }

    private static Set<Character> union(Set<String> set) {
        Set<Character> s = new HashSet<>();
        for (String str : set) {
            s.addAll(strToSet(str));
        }
        return s;
    }

    private static Set<Character> strToSet(String s) {
        return s.chars().mapToObj(c -> (char) c)
            .collect(Collectors.toSet());
    }

    /*
     * DFS based Brute force solution for verification purpose
     */
    public static Set<String> findLowest(Map<String, Integer> plan,
                                         String wish) {
        Set<String> best = empty();
        dfs(plan, wish, empty(), best, Integer.MAX_VALUE);
        return best;
    }

    private static int dfs(Map<String, Integer> plan, String wish,
                           Set<String> res,
                           Set<String> best, int minSofar) {
        if (wish == null || wish.isEmpty()) {
            int cost = costOf(res, plan);
            if (cost < minSofar) {
                best.clear();
                best.addAll(res);
                minSofar = cost;
            }
        } else {
            char p = wish.charAt(0);
            for (String pkg : plan.keySet()) {
                if (pkg.indexOf(p) != -1 && !res.contains(pkg)) {
                    res.add(pkg);
                    minSofar = dfs(plan, wish.substring(1), res, best, minSofar);
                    res.remove(pkg);
                }
            }
        }
        return minSofar;
    }

    private static int costOf(Set<String> pkg, Map<String, Integer> plan) {
        int c = 0;
        for (String p : pkg) {
            c += plan.get(p);
        }
        return c;
    }

    // None recursive Brute force solution

    private static void verify(String wish, Map<Integer, Set<String>> tab,
                               Map<String, Integer> plan) {
        System.out.format("dfs for %s ==> %s\n", wish,
                          findLowest(plan, wish).toString());
    }

    public static void main(String[] args) {
        Map<String, Integer> plan = TEST_PLAN1;
        Map<Integer, Set<String>> tab = dp(plan);
        verify("BAD", tab, plan);
        verify("BAC", tab, plan);
        verify("BCD", tab, plan);
        plan = TEST_PLAN2;
        tab = dp(plan);
        verify("704938521", tab, plan);
    }
}

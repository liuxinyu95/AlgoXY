/*
 * MinFree.java
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

/*
 * The puzzle:
 *   Given a list of non-negative numbers, find the minimum free number,
 *   Which is the minimum one not in this list.
 *
 * Except the brute-force one, all the methods are based on the fact:
 *     answer <= n
 * where n is the length of the array
 */
public class MinFree {

    /* Brute force method */
    static int findMinFree1(int[] nums) {
        int i;
        for(i = 0; contains(i, nums); i++) {}
        return i;
    }

    static boolean contains(int x, int[] nums) {
        for (int n : nums)
            if (x == n) return true;
        return false;
    }

    /* Flag array method */
    static int findMinFree2(int[] nums) {
        int i, n = nums.length;
        boolean[] flags = new boolean[n + 1];
        for (int x : nums)
            if (x <= n)
                flags[x] = true;
        for (i = 0; i < n && flags[i]; i++){}
        return i;
    }

    /* bit-wise flag array */
    final static int N = 1000;

    final static BitSet FLAGS = new BitSet(N);

    static int findMinFree3(int[] nums) {
        int i;
        FLAGS.clear();
        for (int x : nums)
            if (x <= nums.length)
                FLAGS.set(x);
        for (i = 0; i < nums.length && FLAGS.get(i); i++) {}
        return i;
    }

    /* non-recursive divide and conquer */

    static int findMinFree4(int[] nums) {
        int l = 0, u = nums.length;
        while (u - l > 0) {
            int m = l + (u - l) / 2;
            int left, right;
            for (left = right = l; right < u; ++right)
                if (nums[right] <= m)
                    swap(nums, left++, right);
            if (left < m + 1) // left - l < m - l + 1 ==> left isn't full
                u = left;
            else
                l = left;
        }
        return l;
    }

    static final void swap(int[] nums, int i , int j) {
        int t = nums[i];
        nums[i] = nums[j];
        nums[j] = t;
    }

    // verification

    static int[] fromList(List<Integer> xs) {
        return xs.stream().mapToInt(Integer::intValue).toArray();
    }

    static String join(int[] xs, String delim) {
        return IntStream.of(xs).boxed().map(Object::toString)
            .collect(Collectors.joining(delim));
    }

    static void assertEq(int x, int y, int[] nums, String fmt) {
        if (x != y) {
            System.out.format(fmt+"\n[%s]", x, y, join(nums, ", "));
            throw new RuntimeException("assert fail");
        }
    }

    static void test() {
        int k, m;
        List<Integer> xs = IntStream.range(0, N).boxed().collect(Collectors.toList());
        Random gen = new Random();
        for (int i = 0; i < 100; i++) {
            Collections.shuffle(xs);
            int n = gen.nextInt(N);
            int[] nums = fromList(xs.subList(0, n));
            assertEq(findMinFree1(nums), m = findMinFree2(nums), nums,
                     "brute force: %d\tflag array: %d");
            assertEq(findMinFree3(nums), m, nums,
                     "bitset flag: %d\tflag array: %d");
            assertEq(findMinFree4(nums), m, nums,
                     "d&c: %d\tflag array: %d");
        }
        System.out.println("passed 100 tests");
    }

    public static void main(String[] args) {
        test();
    }
}

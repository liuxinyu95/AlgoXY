/*
 * HammingNum.java
 * Copyright (C) 2018 Liu Xinyu (liuxinyu99@hotmail.com)
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
/**
 * Hamming Number
 */

import java.lang.*;
import java.util.*;
import java.util.stream.*;

class HammingNum {
    static long min(long a, long b, long c) {
        return Math.min(a, Math.min(b, c));
    }

    public static long get(int m) {
        long nums[] = new long[m + 1];
        int n = 0, i = 0, j = 0, k = 0;
        nums[0] = 1;
        long x2 = 2 * nums[i];
        long x3 = 3 * nums[j];
        long x5 = 5 * nums[k];
        while (n < m) {
            nums[++n] = min(x2, x3, x5);
            if (x2 == nums[n]) x2 = 2 * nums[++i];
            if (x3 == nums[n]) x3 = 3 * nums[++j];
            if (x5 == nums[n]) x5 = 5 * nums[++k];
        }
        return nums[n - 1];
    }

    public static long getAt(int n) {
        if (n == 1) return 1;
        Queue<Long> q2 = new LinkedList<>(Arrays.asList(new Long[]{2L})),
                    q3 = new LinkedList<>(Arrays.asList(new Long[]{3L})),
                    q5 = new LinkedList<>(Arrays.asList(new Long[]{5L}));
        long x = 1;
        while (n-- > 1) {
            x = min(q2.peek(), q3.peek(), q5.peek());
            if (x == q2.peek()) {
                q2.poll();
                q2.offer(x * 2);
                q3.offer(x * 3);
                q5.offer(x * 5);
            } else if (x == q3.peek()) {
                q3.poll();
                q3.offer(x * 3);
                q5.offer(x * 5);
            } else {
                q5.poll();
                q5.offer(x * 5);
            }
        }
        return x;
    }

    /* helper function to print out collections */
    static String join(long[] xs) {
        return LongStream.of(xs).boxed().map(Object::toString)
            .collect(Collectors.joining(", "));
    }

    static void test() {
        for (int i = 1; i < 100; ++i) {
            long x = get(i), y = getAt(i);
            if (x != y) {
                System.out.format("get(%d) = %d != getAt(%d) = %d\n", i, x, i , y);
                throw new RuntimeException("Assert fail");
            }
        }
    }

    public static void main(String[] args) {
        test();
        System.out.println(get(1500));  //859963392
        long t0 = System.currentTimeMillis();
        long x = getAt(1500);
        long t = System.currentTimeMillis() - t0;
        System.out.format("1500-th = %d, total time = %d[ms]\n", x, t);
    }
}

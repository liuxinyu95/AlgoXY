/*
 * frog.c
 * Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
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
 */
/* 
 * Using short coding style. 
 * a x == b (mod n)
 *  where a = (n - m)
 *        b = (x - y)
 *        n = L
 * the equation is sovlable iff gcd(a, n) | b
 *
 * the problem is that the brute-force method exceeds the time limit. e.g.
 * a = 11; b = 2000000000; L = 2100000000; //answer is 1900000000
 *
 */
typedef long BIG;

BIG gcd(BIG a, BIG b) {
    return a == 0 ? b : gcd(b % a, a);
}

main() {
    BIG a, b, x, y, m, n, L;
    while(scanf("%ld%ld%ld%ld%ld", &x, &y, &m, &n, &L) != -1) {
        a = m < n ? n - m : m - n;
        b = m < n ? x - y : y - x;
        b = (L + b) % L; // avoid negative b
        if ( b % gcd(a, L))
            printf("Impossible\n");
        else {
            for (x = 0, n = 0; x != b; x += a, x -= (x >= L) ? L : 0, ++n);
            printf("%ld\n", n);
        }
    }
}

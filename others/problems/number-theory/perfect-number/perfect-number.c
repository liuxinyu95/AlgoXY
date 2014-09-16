/*
 * perfect-number.c
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
/* Find the perfect number by using Mersonne prime */

#include <stdio.h>
#include <string.h> /* for memset */

#define N 29  /* There are 2^3 = 8 bits in each byte */
#define PRIME(n)  (prime[(n) >> 3] & (1 << ((n) & 0x07)))
#define CLEAR_PRIME(n) prime[(n) >> 3] &= ~ (1 << ((n) & 0x07))

static unsigned char prime[1<<N];

/* Sieve of Eratosthenes */
void sieve(unsigned n) {
    unsigned i, j;
    memset(prime, 0xff, n / sizeof(unsigned char));
    for (i = 2; i*i < n; ++i)
        if (PRIME(i))
            for (j = i*i; j < n; j += i)
                CLEAR_PRIME(j);
}

/*
 * find the largest perfect number not greater than 2^m
 * Euclid-Euler theorem:
 * All the even perfect number can be expressed as
 *    m(m-1)/2, where m-1 is Mersenne prime: m-1 = 2^p - 1, p is prime.
 *
 * In other words:
 *    2^(p-1)*(2^p-1) for prime number p, and Mersenne prime 2^p-1.
 */
unsigned perfect_number(unsigned m) {
    unsigned mp, p = m / 2;
    sieve(1 << p);
    do {
        while (!PRIME(p))
            --p;
        mp = (1<<p) - 1;  /*Mersenne prime: 2^p - 1*/
    } while (!PRIME(mp) && --p);
    return mp * (1<<(p - 1));
}

int main(int argc, char** argv) {
    printf("the largest perfect number < 2^32 is %u\n", perfect_number(32));
    return 0;
}

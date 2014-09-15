/* Find the perfect number by using Mersonne prime */

#include <stdio.h>
#include <stdlib.h> /* for malloc/free */
#include <string.h> /* for memset */

#define N 16

static unsigned prime[1<<N];

/* Sieve of Eratosthenes
 * Find all the prime numbers not greater than 2^m
 */
void sieve(unsigned m) {
    unsigned i, j, n = 1 << m;
    memset(prime, 1, n);
    printf("start sieve proc n = %u, m = %u...\n", n, m);
    for (i = 2; i < n; ++i)
        if (prime[i])
            for (j = i + i; j < n; j += i)
                prime[j] = 0;
    printf("sieve done\n");
    for (i = 2; i < n; ++i)
        if (prime[i])
            printf("%u, ", i);
}

/*
 * find the largest perfect number not greater than 2^m
 * Euclid-Euler theorem:
 * All the even perfect number can be expressed as
 *    m(m-1)/2, where m is Mersenne prime: m = 2^p - 1, p is prime.
 *
 * In other words:
 *    2^(p-1)*(2^p-1) for prime number p, and Mersenne prime 2^p-1.
 */
unsigned perfect_number(unsigned m) {
    unsigned mp, p = m / 2;
    printf("try p=%u\n", p);
    sieve(p);
    do {
        while (!prime[p])
            --p;
        mp = (1<<p) - 1;  /*Mersenne prime: 2^p - 1*/
        printf("p = %u, mp=%u\n", p, mp);
    } while (!prime[mp] && --p);
    return mp * (1<<(p - 1));
}

int main(int argc, char** argv) {
    printf("the largest perfect number < 2^16 is %u\n", perfect_number(16));
    return 0;
}

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

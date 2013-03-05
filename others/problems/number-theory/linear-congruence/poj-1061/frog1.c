/* 
 * Using short coding style. 
 *
 * [1]
 * a x == b (mod n)
 *  where a = (n - m)
 *        b = (x - y)
 *        n = L
 * the equation is sovlable iff gcd(a, n) | b
 *
 * [2]
 * By using extended Euclidean method, gives r*a + s*n = d, where d = gcd(a, n)
 * Then x0 = r*b/d,
 * and all solution x = x0 + k*n/d
 *
 * [1] http://en.wikipedia.org/wiki/Linear_congruence_theorem
 * [2] http://en.wikipedia.org/wiki/Linear_congruence_theorem#Solving_a_linear_congruence
 */
typedef long long BIG;

BIG egcd(BIG a, BIG b, BIG *x, BIG *y) {
    BIG d, x1, y1;
    if (b == 0) {
        d = a;
        *x = 1;
        *y = 0;
    }
    else {
        d = egcd(b, a % b, &x1, &y1);
        *x = y1;
        *y = x1 - a / b * y1;
    }
    return d;
}

main() {
    BIG a, b, x, y, m, n, L, d;
    while(scanf("%lld%lld%lld%lld%lld", &x, &y, &m, &n, &L) != -1) {
        a = m < n ? n - m : m - n;
        b = m < n ? x - y : y - x;
        b = (L + b) % L; // avoid negative b
        d = egcd(a, L, &x, &y);
        if ( b % d)
            printf("Impossible\n");
        else {
            for(x *= b / d, L /= d; x < 0; x += L); /*hanldle negative answer case. e.g. 12 x == 20 (mod 28)*/
            printf("%lld\n", x % L);
        }
    }
}

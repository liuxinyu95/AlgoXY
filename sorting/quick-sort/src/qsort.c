#include <stdio.h>
#include <stdlib.h> // random API for verification purpose only

typedef int Key;

#define swap(x, y) { Key tmp = (x); (x) = (y); (y) = temp; }
#define N 100000

/* 
 * Nico Lumuto parition algorithms. 
 * range [l, u)
 * negate of less than is enough for strict weak order
 */
int partition(Key* xs, int l, int u) {
    int pivot, r;
    for (pivot = l, r = ++l; r < u; ++r)
        if (!(xs[pivot] < xs[r])) {
            ++l;
            swap(xs[l], xs[r]);
        }
    swap(xs[pivot], xs[l]);
    return l + 1;
}

/*
 * range: [l, u)
 */
void qsort(Key* xs, int l, int u) {
    int m;
    if (l < u) {
        m = partition(xs, l, u);
        qsort(xs, l, m);
        qsort(xs, m, u);
    }
}

void testqsort() {
    int i, n, xs[N];
}

void main() {
    testqsort();
}

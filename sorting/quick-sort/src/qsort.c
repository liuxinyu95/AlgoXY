#include <stdio.h>
#include <stdlib.h> // random API for verification purpose only
#include <assert.h>
#include <string.h>

typedef int Key;

//#define N 100000
#define N 20
#define swap(x, y) { Key tmp = (x); (x) = (y); (y) = tmp; }

void printrange(Key* xs, int l, int u) {
    printf("xs[%d, %d) = ", l, u);
    for (; l < u; ++l)
        printf( l == u - 1 ? "%d\n" : "%d, ", xs[l]);
}

/* 
 * Nico Lumuto parition algorithms. 
 * range [l, u)
 * negate of less than is enough for strict weak order
 */
int partition(Key* xs, int l, int u) {
    int r, pivot= l;
    for (r = l + 1; r < u; ++r)
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
void quicksort(Key* xs, int l, int u) {
    int m;
    if (l < u) {
        m = partition(xs, l, u);
        quicksort(xs, l, m - 1);
        quicksort(xs, m, u);
    }
}

/*
 * Robert Sedgewick partition algorithm
 * Based on: 
 * Robert Sedgewick. ``Implementing quick sort programs''. Communication of ACM. pp.847 - 857. Volumn 21. Number 10. 1978.
 */

void qsort1(Key* xs, int l, int u) {
    int i, j, pivot;
    if (l < u - 1) {
        pivot = i = l; j = u;
        while (1) {
            while (i < u && xs[++i] < xs[pivot]);
            while (j >=l && xs[pivot] < xs[--j]);
            if (j < i) break;
            swap(xs[i], xs[j]);
        }
        swap(xs[pivot], xs[j]);
        qsort1(xs, l, j);
        qsort1(xs, i, u);
    }
}

/*
 * Ternery quick sort (3-way partition)
 * Based on:
 * Jon Bentley, M. Douglas McIlroy. ``Engineering a sort function''. Software Practice and experience VOL. 23(11), 1249-1265 1993.
 */
void exchange(Key* xs, int i, int j) {
    Key tmp = xs[i]; xs[i] = xs[j]; xs[j] = tmp;
}

void qsort2(Key* xs, int l, int u) {
    int i, j, k, p, q; Key pivot;
    if (l < u - 1) {
        i = p = l; j = q = u; pivot = xs[l];
        while (1) {
            while (i < u && xs[++i] < pivot);
            while (j >= l && pivot < xs[--j]);
            if (j <= i) break;
            exchange(xs, i, j);
            if (xs[i] == pivot)
                exchange(xs, ++p, i);
            if (xs[j] == pivot)
                exchange(xs, --q, j);
        }
        if (i == j && xs[i] == pivot) { --j, ++i; }
        for (k = l; k <= p; ++k, --j)
            exchange(xs, k, j);
        for (k = u-1; k >= q; --k, ++i)
            exchange(xs, k, i);
        qsort2(xs, l, j + 1);
        qsort2(xs, i, u);
    }
}

/* test */
Key cmp(const void* x, const void* y) {
    return *(Key*)x - *(Key*)y;
}

void testqsort(void (*fsort)(Key*, int, int)) {
    int i, j, n, xs[N], ys[N];
    for (j = 0; j < 100; ++j) {
        for (i = 0, n = rand() % N; i < n; ++i)
            xs[i] = rand() % N;
        memcpy((void*)ys, (const void*)xs, n * sizeof(int));
        printrange(xs, 0, n);
        qsort(xs, n, sizeof(int), cmp);
        fsort(ys, 0, n);
        if (memcmp(xs, ys, n * sizeof(int))) {
            printf("error\n");
            printrange(xs, 0, n);
            printrange(ys, 0, n);
            exit(1);
        }
        //assert(!memcmp(xs, ys, n * sizeof(int)));
    }
}

int main() {
    testqsort(quicksort);
    printf("basic version tested\n");
    testqsort(qsort1);
    printf("sedgewick version tested\n");
    testqsort(qsort2);
    printf("3-way partition tested\n");
    const int xs[] = {2, 7, 3, 7, 9, 0, 12, 3, 9, 9, 17, 0 };
    const int n = sizeof(xs)/sizeof(int);
    int ys[n];
    memcpy((void*)ys, (const void*)xs, sizeof(xs));
    qsort2(ys, 0, n);
    printrange(ys, 0, n);
    return 0;
}

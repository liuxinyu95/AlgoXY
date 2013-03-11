#include <stdio.h>
#include <stdlib.h> // random API for verification purpose only
#include <assert.h>
#include <string.h>

typedef int Key;

#define N 100000
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
    int pivot, r;
    for (pivot = l, r = l + 1; r < u; ++r)
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
    int pivot, i, j;
    if (l < u) {
        pivot = xs[l]; i = l; j = u;
        while (1) {
            do { ++i; } while (xs[i] < pivot);
            do { --j; } while (pivot < xs[j]);
            if (j < i) break;
            swap(xs[i], xs[j]);
        }
        swap(xs[l], xs[j]);
        qsort1(xs, l, j);
        qsort1(xs, i, u);
    }
}

/*
 * An variant of Sedgewick sort algorithm.
 */
void exchange(Key* xs, int i, int j) {
    Key tmp = xs[i]; xs[i] = xs[j]; xs[j] = tmp;
}

void qsort2(Key* xs, int l, int u) {
    int p, i, j;
    if (l < u) {
        for (p = xs[l], i = l + 1, j = u - 1; ;exchange(xs, i++, j--)) {
            while (xs[i] < p) ++i;
            while (p < xs[j]) --j;
            if (j < i) 
                break;
        }
        swap(xs[l], xs[j]);
        qsort2(xs, l, j);
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
        qsort(xs, n, sizeof(int), cmp);
        fsort(ys, 0, n);
        assert(!memcmp(xs, ys, n * sizeof(int)));
    }
}

int main() {
    testqsort(quicksort);
    testqsort(qsort1);
    testqsort(qsort2);
    return 0;
}

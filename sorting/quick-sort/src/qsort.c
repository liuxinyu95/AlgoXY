#include <stdio.h>
#include <stdlib.h> // random API for verification purpose only
#include <assert.h>
#include <string.h>

typedef int Key;

#define swap(x, y) { Key tmp = (x); (x) = (y); (y) = tmp; }
#define N 100000

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

/* test */
Key cmp(const void* x, const void* y) {
    return *(Key*)x - *(Key*)y;
}

void testqsort() {
    int i, j, n, xs[N], ys[N];
    for (j = 0; j < 100; ++j) {
        for (i = 0, n = rand() % N; i < n; ++i)
            xs[i] = rand() % N;
        memcpy((void*)ys, (const void*)xs, n * sizeof(int));
        qsort(xs, n, sizeof(int), cmp);
        quicksort(ys, 0, n);
        assert(!memcmp(xs, ys, n * sizeof(int)));
    }
}

int main() {
    testqsort();
    return 0;
}

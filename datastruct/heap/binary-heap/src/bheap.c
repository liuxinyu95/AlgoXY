#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/*Binary heap with implicit array*/
typedef int Key;

#define N 20

void printn(Key* xs, int n) {
    int i;
    for (i = 0; i < n; ++i)
        printf(i == n - 1 ? "%d\n" : "%d, ", xs[i]);
}

/*Implicit binary tree mapping in array*/

#define PARENT(i) ((((i) + 1) >> 1) - 1)

#define LEFT(i) (((i) << 1) + 1)

#define RIGHT(i) (((i) + 1) << 1)

#define MIN(x, y) (x < y ? x : y)

void swap(Key* a, int i, int j) {
    Key temp = a[i];
    a[i] = a[j];
    a[j] = temp;
}

typedef int (*Less)(Key, Key);
int less(Key x, Key y) { return x < y; }
int notless(Key x, Key y) { return !less(x, y); }

void heapify(Key* a, int i, int n, Less lt) {
    int l, r, m;
    /*printf("i = %d, n = %d\n", i, n);
      printn(a, n);*/
    while (1) {
        l = LEFT(i);
        r = RIGHT(i);
        m = i;
        //printf("l=%d, r=%d, m = %d\n", l, r, m);
        if (l < n && lt(a[l], a[i]))
            m = l;
        if (r < n && lt(a[r], a[m]))
            m = r;
        //printf("i = %d, m=%d\n", i, m);
        if (m != i) {
            swap(a, i, m);
            i = m;
        }
        else
            break;
    }
}

void build_heap(Key* a, int n, Less lt) {
    int i;
    for (i = (n-1) >> 1; i >= 0; --i)
        heapify(a, i, n, lt);
}

Key top(Key* a) { return a[0]; }

/*remove the top element, store it after the end of the array*/
Key pop(Key* a, int n, Less lt) {
    swap(a, 0, --n);
    heapify(a, 0, n, lt);
    return a[n];
}

/*
 * Find the top k elements
 * in-place put the top k elements in array[0...k-1]
 */
int tops(int k, Key* a, int n, Less lt) {
    build_heap(a, n, lt);
    for (k = MIN(k, n) - 1; k; --k)
        heapify(++a, 0, --n, lt);
    return k;
}

void heap_fix(Key* a, int i, Less lt) {
    while (i > 0 && lt(a[i], a[PARENT(i)])) {
        swap(a, i, PARENT(i));
        i = PARENT(i);
    }
}

void decrease_key(Key* a, int i, Key k, Less lt) {
    if (lt(k, a[i])) {
        a[i] = k;
        heap_fix(a, i, lt);
    }
}

/* a[n] should be valid */
void push(Key* a, int n, Key k, Less lt) {
    a[n] = k;
    heap_fix(a, n, lt);
}

/* in-place sort by performing n heapify */
void heap_sort_slow(Key* a, int n) {
    printf("sort: ");
    printn(a, n);
    build_heap(a, n, less);
    printf("heap: ");
    printn(a, n);
    while(--n) {
        printf("top=%d, heap: ", *a);
        heapify(++a, 0, n, less);
        printn(a, n);
    }
}

/* R.W. Floyd heap-sort algorithm */
void heap_sort(Key* a, int n) {
    build_heap(a, n, notless);
    while(n > 1) {
        swap(a, 0, --n);
        heapify(a, 0, n, notless);
    }
}

/* Verification */
int cmp(const void* x, const void* y) {
    return *(Key*)x - *(Key*)y;
}

void test_heapsort(void (*fsort)(Key*, int)) {
    int i, j, n, xs[N], ys[N];
    for (j = 0; j < 100; ++j) {
        for (i = 0, n = rand() % N; i < n; ++i)
            xs[i] = rand() % N;
        memcpy((void*)ys, (const void*)xs, n * sizeof(int));
        heapsort(xs, n, sizeof(int), cmp);
        fsort(ys, n);
        /*assert(!memcmp(xs, ys, n * sizeof(int)));*/
        if(memcmp(xs, ys, n * sizeof(int))) {
            printf("assertion fail!\n");
            printn(xs, n);
            printn(ys, n);
            exit(-1);
        }

    }
}

void test_heapify() {
    Key xs[] = {4, 10, 9, 13, 12, 18, 18};
    heapify(xs, 1, sizeof(xs)/sizeof(xs[0]), less);
    printf("heap(1): ");
    printn(xs, sizeof(xs)/sizeof(xs[0]));
    exit(1);
}

int main(int argc, char** argv) {
    //test_heapify();
    printf("test heap sort slow\n");
    test_heapsort(heap_sort_slow);
    //printf("test heap sort\n");
    //test_heapsort(heap_sort);
    return 0;
}

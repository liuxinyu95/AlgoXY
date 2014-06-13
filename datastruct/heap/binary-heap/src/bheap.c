#include <stdio.h>
#include <stdlib.h>

/*Binary heap with implicit array*/

/*Implicit binary tree mapping in array*/

#define PARENT(i) ((((i) + 1) >> 1) - 1)

#define LEFT(i) (((i)<<1) + 1)

#define RIGHT(i) (((i) + 1) << 1)

#define MIN(x, y) (x < y ? x : y)

#define swap(x, y) { Key temp = x; x = y; y = temp; }

typedef int Key;

typedef int (*Less)(Key, Key);
int less(Key x, Key y) { return x < y; }
int greater(Key x, Key y) { return x > y; }

void heapify(Key* a, int i, int n, Less lt) {
    int l, r, m;
    while (1) {
        l = LEFT(i);
        r = RIGHT(i);
        m = i;
        if (l < n && lt(a[l], a[i]))
            m = l;
        if (r < n && lt(a[r], a[m]))
            m = r;
        if (m != i) {
            swap(a[i], a[m]);
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

Key pop(Key* a, int n, Less lt) {
    Key x = top(a);
    a[0] = a[--n];
    heapify(a, 0, n, lt);
    return x;
}

/*
 * Find the top k elements
 * in-place put the top k elements in array[0...k-1]
 */
int tops(int k, Key* a, int n, Less lt) {
    build_heap(a, n, lt);
    for (k = MIN(k, n) - 1; k; --k)
        heapify(++a, 0, lt);
    return k;
}

void heap_fix(Key* a, int i, Less lt) {
    while (i > 0 && lt(a[i], a[PARENT(i)])) {
        swap(a[i], a[PARENT(i)]);
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
    build_heap(a, n, less);
    while (--n)
        heapify(++a, 0, n, less);
}

/* R.W. Floyd heap-sort algorithm */
void heap_sort(Key* a, int n) {
    build_heap(a, n, greater);
    while(n > 1) {
        swap(a[0], a[n--]);
        heapify(a, 0, greater);
    }
}

/* Verification */
void test_heapsort() {
}

int main(int argc, char** argv) {
    test_heapsort();
}

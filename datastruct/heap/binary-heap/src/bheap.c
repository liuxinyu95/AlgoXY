/*
 * bheap.c
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
    while (1) {
        l = LEFT(i);
        r = RIGHT(i);
        m = i;
        if (l < n && lt(a[l], a[i]))
            m = l;
        if (r < n && lt(a[r], a[m]))
            m = r;
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
 * in-place put the top k elements in array[n-k, n-k+1, ..., n-1]
 */
int tops(int k, Key* a, int n, Less lt) {
    build_heap(a, n, lt);
    for (k = MIN(k, n); k; --k)
        pop(a, n--, lt);
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

/* perform pop n times */
void heap_sort_slow(Key* a, int n) {
    int len = n;
    build_heap(a, n, less);
    while(n)
        pop(a, n--, less);
    while (n < len) /* reverse */
        swap(a, n++, --len);
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

int main(int argc, char** argv) {
    printf("test heap sort slow\n");
    test_heapsort(heap_sort_slow);
    printf("test heap sort\n");
    test_heapsort(heap_sort);
    return 0;
}

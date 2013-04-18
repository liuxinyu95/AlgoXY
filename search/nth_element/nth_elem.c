/*
 * nth_elem.c
 * Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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

/* The k-selection algorithm */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h> 

typedef int Key;

#define N 20 /*100000*/

void printrange(Key* xs, int l, int u) {
    printf("xs[%d, %d) = ", l, u);
    for (; l < u; ++l)
        printf( l == u - 1 ? "%d\n" : "%d, ", xs[l]);
}

void swap(Key* xs, int i, int j) {
    Key tmp = xs[i]; xs[i] = xs[j]; xs[j] = tmp;
}

int partition(Key* xs, int l, int u) {
    int r, p = l;
    for (r = l + 1; r < u; ++r)
        if (!(xs[p] < xs[r]))
            swap(xs, ++l, r);
    swap(xs, p, l);
    return l;
}

/* The result is stored in xs[k], returns k if u-l >=k, otherwise -1 */
int top(int k, Key* xs, int l, int u) {
    int p;
    if (l < u) {
        swap(xs, l, rand() % (u - l) + l);
        p = partition(xs, l, u);
        if (p - l + 1 == k) 
            return p;
        return (k < p - l + 1) ? top(k, xs, l, p) : top(k- p + l - 1, xs, p + 1, u);
    }
    return -1;
}

/* test */
int cmp(const void* x, const void* y) {
    return *(Key*)x - *(Key*)y;
}

void test() {
    int i, j, k, n, xs[N], ys[N];
    for (j = 0; j < 100; ++j) {
        for (i = 0, n = rand() % N + 1; i < n; ++i)
            xs[i] = rand() % N;
        memcpy((void*) ys, (const void*) xs, n * sizeof(int));
        i = rand() % n + 1;
        qsort(xs, n, sizeof(int), cmp);
        k = top(i, ys, 0, n);
        assert( k != -1 && xs[i-1] == ys[k]);
    }
}

int main() {
    test();
    return 0;
}

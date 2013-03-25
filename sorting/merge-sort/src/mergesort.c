/*
 * mergesort.c
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
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

typedef int Key;

#define N 100000
#define INF N + 1

void printrange(Key* xs, int l, int u) {
    printf("xs[%d, %d) = ", l, u);
    for (; l < u; ++l)
        printf( l == u - 1 ? "%d\n" : "%d, ", xs[l]);
}

/*
 * Basic version, using infinity as sentinel
 * Refer to CLRS:
 * Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest and Clifford Stein. 
 * ``Introduction to Algorithms, Second Edition''. ISBN:0262032937. The MIT Press. 2001
 */
void msort(Key* xs, int l, int u) {
    int i, j, m;
    Key *as, *bs;
    if (u - l > 1) {
        m = l + (u - l) / 2;  /* avoid int overflow */
        msort(xs, l, m);
        msort(xs, m, u);
        as = (Key*) malloc(sizeof(Key) * (m - l + 1));
        bs = (Key*) malloc(sizeof(Key) * (u - m + 1));
        memcpy((void*)as, (void*)(xs + l), sizeof(Key) * (m - l));
        memcpy((void*)bs, (void*)(xs + m), sizeof(Key) * (u - m));
        as[m - l] = bs[u - m] = INF;
        for (i = j = 0; l < u; ++l)
            xs[l] = as[i] < bs[j] ? as[i++] : bs[j++];
        free(as);
        free(bs);
    }
}

/*
 * Method described by Knuth in TAOCP
 */
void kmerge(Key* xs, Key* ys, int l, int m, int u) {
    int i, j, k;
    i = k = l; j = m;
    while (i < m && j < u)
        ys[k++] = xs[i] < xs[j] ? xs[i++] : xs[j++];
    while (i < m)
        ys[k++] = xs[i++];
    while (j < u)
        ys[k++] = xs[j++];
    for(; l < u; ++l)
        xs[l] = ys[l]; //memcpy((void*)(xs + l), (void*)(ys + l), sizeof(Key) * (u - l));
}

void kmsort(Key* xs, Key* ys, int l, int u) {
    int m;
    if (u - l > 1) {
        m = l + (u - l) / 2;
        kmsort(xs, ys, l, m);
        kmsort(xs, ys, m, u);
        kmerge(xs, ys, l, m, u);
    }
}

void msort2(Key* xs, int l, int u) {
    Key* ys = (Key*) malloc(sizeof(Key) * (u - l));
    kmsort(xs, ys, l, u);
    free(ys);
}

/* test */
int cmp(const void* x, const void* y) {
    return *(Key*)x - *(Key*)y;
}

void testmsort(void (*fsort)(Key*, int, int)) {
    int i, j, n, xs[N], ys[N];
    for (j = 0; j < 100; ++j) {
        for (i = 0, n = rand() % N; i < n; ++i)
            xs[i] = rand() % N;
        memcpy((void*)ys, (const void*)xs, n * sizeof(int));
        qsort(xs, n, sizeof(int), cmp);
        fsort(ys, 0, n);
        assert(!memcmp(xs, ys, n * sizeof(int)));
        /*
        if(memcmp(xs, ys, n * sizeof(int))) {
            printrange(xs, 0, n);
            printrange(ys, 0, n);
            exit(-1);
        }
        */
    }
}

int main() {
    //testmsort(msort);
    //printf("basic version tested\n");
    testmsort(msort2);
    printf("knuth version tested\n");
    return 0;
}

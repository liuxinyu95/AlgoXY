/*
 * n2_merge_sort.c
 * Copyright (C) 2012 Liu Xinyu (liuxinyu95@gmail.com)
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

/*
 * Nature 2-way merge sort, refer to [3]
 * [1], Donald E Knuth, `The art of computer programming, Volume 3, sorting and searching', 5.2.4
 */

#include <stdio.h> 
#include <stdlib.h>

/* For verification purpose only. */
#include <assert.h>
#define BIG_RAND() (rand() % 10000)

typedef int Key;

void swap(Key** xs, Key** ys) {
    Key* zs = *xs; *xs = *ys; *ys = zs;
}

/*
 * merge xs[a, b) and reverse(xs[c, d)] to ys[k, k+delta, k+2*delta, ...]
 */
int merge(Key* xs, int a, int b, int c, int d, Key* ys, int k, int delta) {
    for(; a < b && c < d; k += delta )
        ys[k] = xs[a] < xs[d-1] ? xs[a++] : xs[--d];
    for(; a < b; k += delta)
        ys[k] = xs[a++];
    for(; c < d; k += delta)
        ys[k] = xs[--d];
    return k;
}

Key* sort(Key* xs, Key* ys, int n) {
    int a, b, c, d, f, r, t;
    if(n < 2)
        return xs;
    for(;;) {
        a = b = 0;
        c = d = n;
        f = 0;
        r = n-1;
        t = 1;
        while(b < c) {
            do {      /* span [a, b) as much as possible */
                ++b;
            } while( b < c && xs[b-1] <= xs[b] );
            do{      /* span [c, d) as much as possible */
                --c;
            } while( b < c && xs[c] <= xs[c-1] );
            if( c < b )
                c = b;   /* eliminate overlap if any */
            if( b - a >= n) 
                return xs;          /* sorted */
            if( t )
                f = merge(xs, a, b, c, d, ys, f, 1);
            else
                r = merge(xs, a, b, c, d, ys, r, -1);
            a = b;
            d = c;
            t = !t;
        } 
        swap(&xs, &ys);
    }
    return 0; /*can't be here*/
}

/* testing */

int sorted(const int* xs, int n){
    int i;
    for(i=0; i<n-1; ++i)
        if(xs[i+1] < xs[i])
            return 0;
    return 1;
}

int check_sum(const int* xs, int n){
    int x = 0;
    while(n>0)
        x ^= xs[--n];
    return x;
}

void test() {
    int i, n, c, m = 100;
    Key *xs, *ys, *zs;
    while(m--) {
        n = BIG_RAND();
        xs = (Key*)malloc(sizeof(Key)*n);
        ys = (Key*)malloc(sizeof(Key)*n);
        for(i=0;  i<n; ++i)
            xs[i] = BIG_RAND();
        c = check_sum(xs, n);
        zs = sort(xs, ys, n);
        assert(sorted(zs, n));
        assert(c == check_sum(zs, n));
        free(xs);
        free(ys);
    }
}
        
int main() {
    test();
    return 0;
}

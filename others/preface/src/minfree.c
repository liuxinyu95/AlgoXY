/*
 * minfree.c, find the minimum free number from a list
 * Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <time.h>
#include "randlist.h"

#define N 1000000
#define WORD_LENGTH (sizeof(int) * 8)

void setbit(unsigned int* bits, unsigned int i){
    bits[i / WORD_LENGTH] |= 1 << (i % WORD_LENGTH);
}

int testbit(unsigned int* bits, unsigned int i){
    return bits[i/WORD_LENGTH] & (1 << (i % WORD_LENGTH));
}

unsigned int bits[N / WORD_LENGTH + 1];

int ids[N];

void init_ids() {
    int i;
    for(i = 0; i < N; ++i)
        ids[i] = i;
}

// min-free, brute force version
int brute_force(int* xs, int n){
    int i, j;
    for(i = 0; ; ++i) {
        int in = 0;
        for(j = 0; j < n; ++j)
            if(xs[j] == i) {
                in = 1;
                break;
            }
        if(!in) return i;
    }
}

// min-free, flags version
int flag_map(int*xs, int n) {
    int* flags = (int*) malloc(sizeof(int) * (n + 1));
    int i;
    for(i = 0; i <= n; ++i)
        flags[i] = 0;
    for(i = 0; i < n; ++i)
        if(xs[i] < n)
            flags[xs[i]] = 1;
    for(i = 0; i <= n; ++i)
        if(!flags[i]) {
            free(flags);
            return i;
        }
    return n; //shouldn't be here
}

// min-free, bitmap version
int bitmap_min_free(int* xs, int n) {
    int i, len = N/WORD_LENGTH+1;
    for(i = 0; i < len; ++i)
        bits[i]=0;
    for(i = 0; i < n; ++i)
        if(xs[i] < n)
            setbit(bits, xs[i]);
    for(i = 0; i <= n; ++i)
        if(!testbit(bits, i))
            return i;
    return n; //shouldn't be here
}

/* a small fine tuned version */
int bitmap_min_free1(int* xs, int n){
    int i, j, len = N / WORD_LENGTH + 1;
    for(i = 0; i < len; ++i)
        bits[i] = 0;
    for(i = 0; i<n; ++i)
        if(xs[i] < n)
            setbit(bits, xs[i]);
    for(i = 0; ; ++i)
        if(~bits[i] !=0 )
            for(j = 0; ; ++j)
                if(!testbit(bits, i * WORD_LENGTH + j))
                    return i * WORD_LENGTH+j;
    return n; //shouldn't be here
}

// binary search
//  xs: array, n: length of xs
//  l: lower bound, u: upper bound
int binary_search(int* xs, int n, int l, int u){
    if(n==0) return l;
    int m = (l + u) / 2;
    int right, left = 0;
    //0 ... <=m ... left ... >m ... right ...? ...
    for(right = 0; right < n; ++ right)
        if(xs[right] <= m){
            swap(xs[left], xs[right]);
            ++left;
        }
    if(left == m - l + 1)
        return binary_search(xs+left, n-left, m+1, u);
    else
        return binary_search(xs, left, l, m);
}

// min-free, divide and conquer, with recursion.
int dc_min_free(int* xs, int n){
    return binary_search(xs, n, 0, n-1);
}

// min-free, divide and conquer, eliminate recursion.
int dc_min_free_iter(int* xs, int n){
    int l=0;
    int u=n-1;
    while(n){
        int m = (l + u) / 2;
        int right, left = 0;
        //0 ... <=m ... left ... >m ... right ...? ...
        for(right = 0; right < n; ++ right)
            if(xs[right] <= m){
                swap(xs[left], xs[right]);
                ++left;
            }
        if(left == m - l + 1){
            xs = xs + left;
            n  = n - left;
            l  = m+1;
        }
        else{
            n = left;
            u = m;
        }
    }
    return l;
}

/* Verification part */

typedef int (*MinFree)(int*, int);

double measure(MinFree f, int* xs, int y) {
    clock_t start = clock();
    f(xs, y);
    return ((double) (clock() - start)) / CLOCKS_PER_SEC;
}

double measure_min_free(MinFree f){
    double t, tm = 0.0;
    int i;
    printf("start measure min_free...\n");
    for(i = 0; i < 100; ++i){
        shuffle(ids, N);
        t = measure(f, ids, N-1);
        /* printf("%f[s] elapsed\n", t); */
        tm = tm + t;
    }
    return tm / 100.00;
}



int verify_min_free(MinFree f){
    int i;
    printf("verify against brute-force method...\n");
    for(i=0; i < 10; ++i){
        shuffle(ids, 100);
        if(brute_force(ids, 99) != f(ids, 99)){
            printf("failed\n");
            return 0;
        }
    }
    printf("OK\n");
    return 1;
}

void verify(){
    printf("verify flags method: %d\n", verify_min_free(flag_map));
    printf("verify bitmap method: %d\n", verify_min_free(bitmap_min_free));
    printf("verify bitmap1 method: %d\n", verify_min_free(bitmap_min_free1));
    printf("verify divide & conquer method: %d\n", verify_min_free(dc_min_free));
    printf("verify divide & conquer iter method: %d\n", verify_min_free(dc_min_free_iter));
}

int main(){
    init_ids();

    verify();

    /* printf("average time of brute-force method: %f[s]\n", */
    /* 	 measure_min_free(brute_force)); */

    printf("average time of flags method: %f[s]\n",
           measure_min_free(flag_map));

    printf("average time of bitmap method: %f[s]\n",
           measure_min_free(bitmap_min_free));

    printf("average time of divide and conquer: %f[s]\n",
           measure_min_free(dc_min_free));

    printf("average time of divide and conquer without recursion: %f[s]\n",
           measure_min_free(dc_min_free_iter));

    return 0;
}

/*
 * Some performance data
 *
 * n = 100,000   0.1 million
 * brute force: 5.4 [s]
 * flags: average time of flags method: 0.001860[s]
 * bitmap: average time of bitmap method: 0.001710[s]
 *
 * n = 1,000,000   1 million
 * brute force: ???
 * flags: average time of flags method: 0.034960[s]
 * bitmap: average time of bitmap method: 0.022630[s]
 */

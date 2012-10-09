/*
 * bralist.c
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
 * Binary random access list based on [1]
 * [1]. Chris Okasaki. ``Purely Functional Random-Access Lists''. 
 * Functional Programming Languages and Computer Architecutre, June 1995, pages 86-95.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h> /* for memcpy */
#include <assert.h> /* for assertion */

#define BIG_RAND() rand() % 1000

#define M sizeof(int)*8      /* Limit the length of the list to 2^(M+1) - 1 */
typedef int Key;  /* Alternatively,  C++ template can be used */

struct List {
  int n;
  Key* tree[M];
};

struct List empty() {
  int i;
  struct List a;
  a.n = 0;
  for(i = 0; i < M; ++i)
    a.tree[i] = NULL;
  return a;
}

void release(struct List a) {
  int i;
  for(i = 0; i < M; ++i)
    free(a.tree[i]);   /* free(NULL) has no side effect */
}

int size(struct List a) {
  return a.n;
}

/* returns number of bits of n */
int nbits(int n) {
  int i=0;
  while(n >>= 1)
    ++i;
  return i;
}

/*
 * Aggregate performance analysis.
 */
struct List insert(struct List a, Key x) {
  int i, j, sz;
  Key* xs;
  i = nbits( (a.n+1) ^ a.n ); /* xor helps locate the bit flip to 1 r*/
  xs = a.tree[i] = (Key*)malloc(sizeof(Key)*(1<<i));
  for(j=0, *xs++ = x, sz = 1; j<i; ++j, sz <<= 1) {
    memcpy((void*)xs, (void*)a.tree[j], sizeof(Key)*(sz));
    xs += sz;
    free(a.tree[j]);
    a.tree[j] = NULL;
  }
  ++a.n;
  return a;
}

/* 
 * O(\lg N) random access 
 * index out of bound error is omit here
 * suppose i < size(a)
 */
Key get(struct List a, int i) {
  int j, sz;
  for(j = 0, sz = 1; j < M; ++j, sz <<= 1)
    if(a.tree[j]) {
      if(i < sz)
	break;
      i -= sz;
    }
  return a.tree[j][i];
}

void print_array(Key* xs, int n) {
  int i;
  for(i=0; i<n; ++i)
    printf( i == n -1 ? "%d\n" : "%d, ", xs[i]);
}

void print_list(struct List a) {
  int i;
  for(i=0; i<size(a); ++i)
    printf( i == size(a) -1 ? "%d\n" : "%d, ", get(a, i));
}

void print_trees(struct List a) {
  int i, j, m, n;
  printf("size: %d:\n", a.n);
  m = nbits(a.n);
  for(i=0; i<=m; ++i) {
    printf("tree[%d]:", i);
    if(a.tree[i]) {
      n = 1<<i;
      for(j=0; j<n; ++j)
	printf(j==n-1 ? "%d\n" : "%d, ", a.tree[i][j]);
    }
    else
      printf("NIL\n");
  }
  printf("\n");
}

void assert_eq(Key* xs, struct List ys, int n) {
  int i;
  for(i = 0; i<n; ++i)
    assert(xs[i] == get(ys, i));
}

void test_insert() {
  int i, n, m=1000;
  Key* xs;
  struct List ys;
  while(m--){
    n = BIG_RAND();
    xs = (Key*)malloc(sizeof(Key)*n);
    for(i = 0; i<n; ++i)
      xs[i] = BIG_RAND();
    ys = empty();
    for(i=n-1; i>=0; --i)
      ys = insert(ys, xs[i]);
    assert_eq(xs, ys, n);
    release(ys);
    free(xs);
  }
}

int main(int argc, char** argv) {
  test_insert();
  return 0;
}

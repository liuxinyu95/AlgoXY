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

#define M 32      /* Limit the length of the list to 2^(M+1) - 1 */
typedef int Key;  /* Alternatively,  C++ template can be used */

struct List {
  int n;
  Key* tree[M];
};

struct List empty() {
  int i;
  List a;
  a.n = 0;
  for( i = 0; i < M; ++i)
    a.tree[i] = NULL;
  return a;
}

void release(List a) {
  for( i = 0; i < M; ++i)
    free(a.tree[i]);   /* free(NULL) has no side effect */
}

int size(List a) {
  return a.n;
}

/* returns number of bits of n */
int nbits(int n) {
  int i=1;
  while( n >>= 1)
    ++i;
  return i;
}

struct List insert(List a, Key x) {
  int i, j = 0, k, n = ++a.n;
  Key* xs;
  while( (i = nbits(n)) && a.tree[i])
    n = n & ((1<<i) - 1); /* remove MSB by bit-and 2^i - 1 */
  xs = a.tree[i] = (Key*)malloc(sizeof(Key)*(1<<i));
  while(--i) {
    memcpy(xs, a.tree[i], sizeof(Key)*(1<<i));
    xs += (1<<i);
    free(a.tree[i]);
    a.tree[i] = NULL;
  }
  *xs = x;
  return a;
}

Key get(List a, int i) {
  
}

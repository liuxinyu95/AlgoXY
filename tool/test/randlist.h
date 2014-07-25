/*
 * randlist.h, Random helper tool
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

#include <stdlib.h>

#define swap(x, y) {int t=x; x=y; y=t;}
#define bigrand rand

// randint(a, b)
//  Generate an integer in range [a, b]
int randint(int a, int b){
  return bigrand() % (b - a + 1) + a;
}

// gen-knuth
//   Same as gen_knuth, but user should allocate
//   memory in advance.
void gen_knuth(int* xs, int m, int n){
  int i;
  for(i = 0; i < n; ++i)
    if(bigrand() % (n - i) < m){
      xs[m-1] = i;
      --m;
    }
}

// get-knuth
//  Generate m random integers in [0, n)
//  Return an array of m elements (without ordering).
//  User need free the memory of array later on.
//  Slow due to malloc
int* get_knuth(int m, int n){
  int* xs = (int*)malloc(sizeof(int)*m);
  gen_knuth(xs, m, n);
  return xs;
}

// shuffle(list)
//  In-place shuffle a list of numbers
void shuffle(int* xs, int n){
  int m = n >> 1;
  int i;
  for(i = 0; i < m; ++i){
    int j = randint(i, n-1);
    swap(xs[i], xs[j]);
  }
}

// sample(list, k)
//  Random select k elements from a list
//  The result is stored in first k element in list
//  User need allocate memory in advance
void sample(int* xs, int n, int k){
  int i, j=0;
  for(i = 0; i < n; ++i)
    if( bigrand() % (n-i) < k){
      swap(xs[j], xs[i]);
      --k;
      ++j;
    }
}

// get_sample(list, k)
//  Random select k elements from a list
//  Return an array of k random numbers without ordering
//  User need free the array later on
//  Slow due to malloc
int* get_sample(int* xs, int n, int k){
  int* index = get_knuth(k, n);
  int* ns = (int*) malloc(sizeof(int)*k);
  while(--k)
    ns[k]=xs[index[k]];
  free(index);
  return ns;
}

// choice(list)
//  Random select an element from list
int choice(int* xs, int n){
  return xs[bigrand() % n];
}

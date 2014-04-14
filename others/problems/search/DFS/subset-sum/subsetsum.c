/*
 * subsetsum.c
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

//convert x from decimal to n digits of m numeric system
void to_digit(int x, int m, int* w, int n){
  int i;
  for(i=0; i<n; ++i){
    w[i] = x % m;
    x = x / m;
  }
}

int power(int x, int n){
  int m=1;
  while(n--)
    m *= x;
  return m;
}

/**
 * a Naive Brute-force method
 *
 * to solve sum(select_from(A)) == s
 * Since for all element a in A, we have a >=1
 * We at most select one element s times, and
 * at least select it 0 times.
 * so it is in range [0, s+1).
 * The idea is to have a N digits (s+1) numeric
 * system, loop from 0 to (s+1)^N where |A| = N
 * For every number w = [w[n-1], w[n-2], ..., w[0]]
 * we check if sum(w[i]*a[i]) == s
 */

void solve(const int* a, int n, int s){
  int* w = (int*) malloc(n * sizeof(int));
  int i, j, x, m;
  m=power(s+1, n);
  for(i=0; i<m-1; ++i){
    to_digit(i, (s+1), w, n); 
    x = 0;
    for(j=0; j<n; ++j)
      x += a[j]*w[j];
    if(x == s){
      for(j=0; j<n; ++j)
	while(w[j]--) printf("%d, ", a[j]);
      printf("\n");
    }
  }
  free(w);
}

/**
 * DFS method 1
 *
 * Everytime, we pick one candidate from A, then
 * recursively pick the rest from A until we have
 * all picked numbers sum to s.
 *
 * Demerit:  
 * 1. Although it's intuitive, the search
 * depths is propotion to s, if |A| is far less
 * than s, it performs bad.
 * Example: A=[1], s=10000
 * 2. It output duplicate results, actually
 * the permutation of the answer. And we needn't
 * care about the order of selected elements
 */
void dfs(const int* a, int n, int s){
  int i, j, depth=0;
  int back;
  int* w = (int*) malloc( (s+1)* sizeof(int) );
  int* z = (int*) malloc( (s+1)* sizeof(int) );
  z[depth]=0;
  for(i=0; i<s+1; ++i)
    w[i] = -1;

  while(depth>=0){
    back = 1;
    for(i=w[depth]+1; i<n; ++i){
      if(a[i]+z[depth] <= s){
	back = 0;
	w[depth] = i;
	z[depth+1] = z[depth]+a[i];
	w[++depth] = -1;
	if(z[depth] == s){ //output 1 solution
	  for(j=0; j<depth; ++j)
	    printf("%d, ", a[w[j]]);
	  printf("\n");
	  back = 1;
	  --depth;
	}
	else
	  break;
      }
    }
    if(back)
      --depth;
  }
  free(z);
  free(w);
}

/**
 * DFS method 2
 * Suppose a = filter((<s), a)
 */
void dfs2(const int* a, int n, int s){
  int i, j, k, depth=0;
  int back;
  int* w = (int*)malloc( n * sizeof(int) );
  int* z = (int*)malloc( (n+1) * sizeof(int) );
  for(i=0; i<n; ++i)
    w[i] = -1;
  z[0] = 0;

  while(depth >= 0){
    back = 1;
    for(i=w[depth]+1; a[depth]*i+z[depth] <= s; ++i){
      back = 0;
      w[depth] = i;
      z[depth+1] = z[depth] + a[depth]*i;
      if(z[depth+1] == s){ //output 1 solution
	for(j=0; j<=depth; ++j)
	  for(k=0; k<w[j]; ++k)
	    printf("%d, ", a[j]);
	printf("\n");
      }
      else if(depth < n-1){
	w[++depth] = -1;
	break;
      }
    }
    if(back)
      --depth;
  }
  free(z);
  free(w);
}

int main(int argc, char** argv){
  int i, x;
  const int a[] = {1,2,3,4,5,6,7,8,9,10};
  //solve(a, 10, 5);
  //dfs(a, 10, 5);
  dfs2(a, 10, 5);
}

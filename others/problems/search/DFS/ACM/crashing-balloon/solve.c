/*
 * solve.c
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

/*
 * Solution 1
 *   Suppose a > b, 
 *  for any valide factor decomposition of b, there exists a valid decomposition of a => a
 *  Otherwise => b
 *
 * Note that the ZOJ doesn't use this rule, solution 1 won't pass online judge, but I do
 *    Think solution 1 has its value.
 */

int valid(int a, int b, int n) {
  if( n == 1 )
    return a ==1 || b != 1;
  else
    if( b % n == 0 && !valid(a, b/n, n-1))
      return 0;
    else if( a % n == 0 && valid(a/n, b, n-1))
      return 1;
    else
      return valid(a, b, n-1);
}

/*
 * Solution 2: (This can pass ZOJ)
 *  If there exists a decomposition for both a and b ==> a
 *  else if there exists a decomposition for b ==> b
 *  otherwise ==> a
 */
int both_OK , b_OK;

/* This is essentaily DFS */
int exist(int a , int b , int n) {
  if(both_OK)
    return 1;

  if(a==1 && b==1)
    return both_OK = b_OK = 1;

  if(b==1)
    b_OK = 1;

  if(n==1) 
    return both_OK;

  /*Alternatively, we can use for loop, refer to the python ver.*/
  if (a % n == 0 && exist(a/n, b, n-1))
    return 1;
  if (b % n == 0 && exist(a, b/n, n-1))
    return 1;
  return exist(a, b, n-1);
}

void swap(int* a, int* b) {
  int tmp = *a; *a = *b; *b = tmp;
}

int main(){
  int a, b;
  while(scanf("%d %d", &a, &b)!=EOF) {
    if(b > a)
      swap(&a, &b);
    both_OK = b_OK = 0;
    exist(a, b, 100);
    printf("%d\n", both_OK || !b_OK ? a : b);
    /* printf("%d\n", valid(a, b, 100) ? a : b); */ /*Solution 1*/
  }
  return 0;
}

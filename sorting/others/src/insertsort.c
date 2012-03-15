/*
 * insertsort.c
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

#include <stdio.h>
#include <stdlib.h>

/* For verification purpose only. */
#include <assert.h>

#define BIG_RAND() (rand() % 10000)

/* For illustration purpose, assume elements are integers*/

typedef int Key;

void swap(Key* xs, int i, int j){
  Key temp = xs[i];
  xs[i] = xs[j];
  xs[j] = temp;
}

void isort(Key* xs, int n){
  int i, j;
  for(i=1; i<n; ++i)
    for(j=i-1; j>=0 && xs[j+1] < xs[j]; --j)
      swap(xs, j, j+1);
}

/* Linked list version*/

struct node{
  Key key;
  struct node* next;
};

struct node* insert(struct node* lst, struct node* x){
  struct node *p, *head;
  p = NULL;
  for(head = lst; lst && x->key > lst->key; lst = lst->next)
    p = lst;
  x->next = lst;
  if(!p)
    return x;
  p->next = x;
  return head;
}

struct node* isort1(struct node* lst){
  struct node *p, *r;
  r = NULL;
  while(lst){
    p = lst;
    lst = lst->next;
    r = insert(r, p);
  }
  return r;
}

/* Auxiliary functions */

struct node* to_list(const Key* xs, int n){
  struct node* p;
  if(n==0)
    return NULL;
  p=(struct node*)malloc(sizeof(struct node));
  p->key = xs[0];
  p->next = to_list(xs+1, n-1);
  return p;
}

void to_array(struct node* lst, Key* xs, int n){
  int i;
  for(i=0; i<n; ++i, lst = lst->next)
    xs[i] = lst->key;
}

void release_list(struct node* lst){
  struct node* p;
  while(lst){
    p = lst;
    lst = lst->next;
    free(p);
  }
}

/* Verification */

int sorted(const Key* xs, int n){
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

/* 
 * As insertion sort is bound to O(N^2), we limit the number of
 * test to save time
 */
void test_sort(){
  int m = 100;
  int i, n, c, *xs;
  while(m--){
    n = 1 + BIG_RAND();
    xs = (int*)malloc(sizeof(int)*n);
    for(i=0; i<n; ++i)
      xs[i] = BIG_RAND();
    c = check_sum(xs, n);

    /* test array version*/
    isort(xs, n);
    assert(sorted(xs, n));
    assert(c == check_sum(xs, n));

    /* test linked-list version*/
    struct node* ys = to_list(xs, n);
    ys = isort1(ys);
    to_array(ys, xs, n);
    assert(c == check_sum(xs, n));
    release_list(ys);

    free(xs);
  }
  printf("OK\n");
}

int main(){
  test_sort();
  return 0;
}

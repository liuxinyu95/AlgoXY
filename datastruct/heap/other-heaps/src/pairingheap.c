/*
 * fibheap.c, Fibonacci heap
 * Copyright (C) 2012, Liu Xinyu (liuxinyu95@gmail.com)

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

/*
 * Based on Michael L. Fredman, Robert Sedgewick, Daniel D. Sleator, 
 * and Robert E. Tarjan. ``The Pairing Heap: A New Form of Self-Adjusting 
 * Heap'' Algorithmica (1986) 1: 111-129
 */

#include <stdio.h>
#include <stdlib.h>

/* For verification purpose only. */
#include <assert.h> 

#define BIG_RAND()  (rand() % 10000) 
/* End of verification purpose only part */

typedef int Key;

/* 
 * Definition of K-ary tree node
 * left child, right sibling approach.
 */
struct node{
  Key key;
  struct node *next, *children;
};

/* Auxiliary functions */
struct node* singleton(Key x){
  struct node* t = (struct node*)malloc(sizeof(struct node));
  t->key = x;
  t->next = t->children = NULL;
  return t;
}

/* 
 * Helper function to release a heap recursively 
 * Do nothing for empty heap.
 */
void destroy_heap(struct node* h){
  struct node* t;
  if(h)
    while((t=h->children) != NULL){
      h->children = h->children->next;
      destroy_heap(t);
    }
  free(h);
}

/* Swap 2 heaps */
void swap(struct node** x, struct node** y){
  struct node* p = *x;
  *x = *y;
  *y = p;
}

/* Insert a new element in front of a linked-list, O(1) */
struct node* push_front(struct node* lst, struct node* x){
  x->next = lst;
  return x;
}

/* 
 * Merge
 * O(1) time by inserting one heap as the first
 * child of the other.
 */
struct node* merge(struct node* h1, struct node* h2){
  if(h1 == NULL)
    return h2;
  if(h2 == NULL)
    return h1;
  if(h2->key < h1->key)
    swap(&h1, &h2);
  h2->next = h1->children;
  h1->children = h2;
  h1->next = NULL; /*Break previous link if any*/
  return h1;
}

/* Insertion, O(1) time*/ 
struct node* insert(struct node* h, Key x){
  return merge(h, singleton(x));
}

/* Top, finding the minimum element. O(1) time */
Key top(struct node* h){
  return h->key;
}

/* 
 * Pop, delete the minimum element.
 * O(\lg N) amortized time in conjecture.
 */
struct node* pop(struct node* h){
  struct node *x, *y, *lst = NULL;
  while((x = h->children) != NULL){
    if((h->children = y = x->next) != NULL)
      h->children = h->children->next;
    lst = push_front(lst, merge(x, y));
  }
  x = NULL;
  while((y = lst) != NULL){
    lst = lst->next;
    x = merge(x, y);
  }
  free(h);
  return x;
}

void heap_sort(int* xs, int n){
  int i;
  struct node* h = NULL;
  for(i=0; i<n; ++i)
    h = insert(h, xs[i]);
  for(i=0; i<n; ++i){
    xs[i] = top(h);
    h = pop(h);
  }
  destroy_heap(h);
}

/* Testing */
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

void test_heap_sort(){
  int m = 1000;
  int i, n, c, *xs;
  while(m--){
    n = 1 + BIG_RAND();
    xs = (int*)malloc(sizeof(int)*n);
    for(i=0; i<n; ++i)
      xs[i] = BIG_RAND();
    c = check_sum(xs, n);
    heap_sort(xs, n);
    assert(sorted(xs, n));
    assert(c == check_sum(xs, n));
    free(xs);
  }
}

int main(){
  test_heap_sort();
  return 0;
}

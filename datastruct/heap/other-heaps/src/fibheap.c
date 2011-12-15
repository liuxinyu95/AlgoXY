/*
 * fibheap.c, Fibonacci heap
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

/**
 * Based on Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest 
 * and Clifford Stein. ``Introduction to Algorithms, Second Edition.''
 *  Chapter 20, Fibonacci heaps. The MIT Press 2001, ISBN:0262032937
 */

#include <stdlib.h>
#include <stdio.h>

typedef int Key;

/* Definition of tree */
struct node{
  Key key;
  struct node *next, *prev, *parent, *children;
  int degree; /* As known as rank */
  int mark;
};

struct FibHeap{
  struct node *roots;
  struct node *minTr; /* Point to tree contains the minimum element in root */
  int n; /* number of nodes */
};

/* Auxiliary functions */
struct node* singleton(Key x){
  struct node* t = (struct node*)malloc(sizeof(struct node));
  t->key = x;
  t->parent = t->children = 0;
  t->prev = t->next = t;
  t->degree = 0;
  t->mark = 0;
  return t;
}

/* create an empty heap */
struct FibHeap* empty(){
  struct FibHeap* h = (struct FibHeap*)malloc(sizeof(struct FibHeap));
  h->roots = 0;
  h->minTr = 0;
  h->n = 0;
  return h;
}

struct node* concat(struct node* first1, struct node* first2){
  struct node* last1 = first1->prev;
  struct node* last2 = first2->prev;
  last1->next = first2;
  first2->prev = last1;
  last2->next = first1;
  first1->prev = last2;
  return first1;
}

struct node* append(struct node* first, struct node* x){
  if(!first)
    return x;
  struct node* last = first->prev;
  last->next = x;
  x->prev = last;
  x->next = first;
  first->prev = x;
  return first;
}

struct FibHeap* add_tree(struct FibHeap* h, struct node* t){
  if(!h)
    h = empty();
  h->roots = append(h->roots, t);
  return h;
}

/* Insertion. O(1) */
struct FibHeap* insert(struct FibHeap* h, Key x){
  struct node* t = singleton(x);
  h = add_tree(h, t);
  if(h->minTr==0 || h->minTr->key < x)
    h->minTr = t;
  h->n++;
  return h;
}

/* top, finding the minimum element. O(1) */
Key top(struct FibHeap* h){
  return h->minTr->key;
}

/* merge. O(1)
 * note that the two heaps passed in are destroyed after merging.
 */
struct FibHeap* merge(struct FibHeap* h1, struct FibHeap* h2){
  struct FibHeap* h;
  if(!h1)
    return h2;
  if(!h2)
    return h1;
  h = empty();
  h->roots = concat(h1->roots, h2->roots);
  if(h1->minTr->key < h2->minTr->key)
    h->minTr = h1->minTr;
  else
    h->minTr = h2->minTr;
  h->n = h1->n + h2->n;
  free(h1);
  free(h2);
  return h;
}

int main(){
  return 0;
}

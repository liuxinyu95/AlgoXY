/*
 * list.cpp
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
#include <iostream>
#include <cstdlib>
#include <cstdio>

using namespace std;

/* 
 * A pure imperative list implementation WITH some optimization such as
 * tail record etc.
 */

/* Definition of node. */
template<typename T>
struct Node {
  T key;
  Node* next;

  Node(): next(0) {}
  Node(T x) : key(x), next(0) {}
  Node(T x, Node* n) : key(x), next(n) {}
  ~Node() { delete next; }
};

/* Definition of list with a tail pointer to speed up appending etc. */
template<typename T>
struct List {
  Node<T>* head;
  Node<T>* tail;

  List() : head(0), tail(0) {}
  List(Node<T>* h, Node<T>* t) : head(h), tail(t) {}
};

/* Auxiliar functions help to wrap a list from a head and a tail pointers. */
template<typename T>
List<T> make(Node<T>* head, Node<T>* tail) {
  if (!tail || !head) /* Trick: it actualy handles (nil, nil), and (_, nil) cases, the later is a singleton. */
    tail = head;
  return List<T>(head, tail);
}

template<typename T>
void release(List<T> lst) {
  delete lst.head;
}

/* Test if a list is empty. */
template<typename T>
bool empty(List<T> lst) { return !lst.head; }

/* Create an empty list of a given type. */
template<typename T>
List<T> empty() { return List<T>(); }

/* Access the first element in the list. */
template<typename T>
T first(List<T> lst) { return lst.head->key; }

/* Access the rest sub-list (a list contains all elements except for the first one). */
template<typename T>
List<T> rest(List<T> lst) { 
  return make(lst.head->next, lst.tail);
}

/* 'cons' a list from an element and a list, follow the Lisp naming tradition. */
template<typename T>
List<T> cons(T x, List<T> lst) {
  return make(new Node<T>(x, lst.head), lst.tail);
}

/* 
 * Calculate the length, not optimized, Linear O(N) time
 * One optimization is to record the length, and update it wile mutating the list.
 */
template<typename T>
int length(List<T> lst) {
  int n = 0;
  for (Node<T>* xs = lst.head; xs; ++n, xs = xs->next);
  return n;
}

/* 
 * Random access, O(N) time bound.
 * Assume 0 <= n < length(lst) 
 * The bound check is skipped.
 */
template<typename T>
T getAt(List<T> lst, int n) {
  Node<T>* xs = lst.head;
  while (n--)
    xs = xs->next;
  return xs->key;
}

/*
 * Appending with the help of tail pointer, O(1) constant time.
 */
template<typename T>
List<T> append(List<T> lst, T x) {
  Node<T>* n = new Node<T>(x);
  if (empty(lst))
    return make(n, n);
  lst.tail->next = n;
  return make(lst.head, n);
}

/* Concatenation with the help of tail pointer, O(1) constant time. */
template<typename T>
List<T> concat(List<T> lst1, List<T> lst2) {
  lst1.tail->next = lst2.head;
  return lst1;
}

/*
 * Access the last element, O(1) with the help of tail pointer.
 * Suppose the input list isn't empty. 
 */
template<typename T>
T last(List<T> lst) {
  return lst.tail->key;
}

/* 
 * Get a sub list contains all elements except the last one, O(N) time.
 * Suppose the input list isn't empty. 
 */
template<typename T>
List<T> init(List<T> lst) {
  List<T> r = empty<T>();
  for(Node<T>* xs = lst.head; xs != lst.tail; xs = xs->next)
    r = append(r, xs->key);
  return r;
}

/* Testing purpose only*/
template<typename T>
void print_lst(List<T> lst) {
  for(Node<T>* xs = lst.head; xs; xs = xs->next)
    cout<<xs->key<<", ";
  cout<<"\n";
}

int main(int, char**) {
  int i, n;
  List<int> lst = cons(1, cons(2, cons(3, empty<int>())));
  printf("length of empty = %d, len([1, 2, 3]) = %d\n", length<int>(empty<int>()), length(lst));
  lst = append(lst, 4);
  printf("append 4 ==>");
  print_lst(lst);
  n = length(lst);
  for(i = 0; i < n; ++i)
    printf("lst[%d] = %d%s", i, getAt(lst, i), i == n-1 ? "\n" : ", ");
  printf("last(lst) = %d, init(lst) = ", last(lst));
  print_lst(init(lst));

  lst = concat(lst, cons(5, cons(6, cons(7, empty<int>()))));
  print_lst(lst);

  release(lst);
}

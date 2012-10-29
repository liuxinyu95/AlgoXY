#include <iostream>
#include <cstdlib>

using namespace std;

/* 
 * A pure imperative list implementation WITHOUT ANY optimization such as
 * tail record etc.
 */

/* Definition */
template<typename T>
struct List {
  T key;
  List* next;

  List() : next(0) {}
  ~List() { delete next; }
};

/* Access the rest sub-list (a list contains all elements except for the first one). */
template<typename T>
T first(List<T> *xs) { return xs->key; }

/* Access the rest sub-list (a list contains all elements except for the first one). */
template<typename T>
List<T>* rest(List<T>* xs) { return xs->next; }

/* 'cons' a list from an element and a list, follow the Lisp naming tradition. */
template<typename T>
List<T>* cons(T x, List<T>* xs) {
  List<T>* lst = new List<T>;
  lst->key = x;
  lst->next = xs;
  return lst;
}

/* 
 * Calculate the length, not optimized, Linear O(N) time
 */
template<typename T>
int length(List<T>* xs) {
  int n = 0;
  for (; xs; ++n, xs = xs->next);
  return n;
}

/* 
 * Random access, O(N) time
 * Assume 0 <= n < length(lst) 
 * The bound check is skipped.
 */
template<typename T>
T getAt(List<T>* xs, int n) {
  while (n--)
    xs = xs->next;
  return xs->key;
}

/*
 * Appending an element to the end of a list, O(N) linear time
 */
template<typename T>
List<T>* append(List<T>* xs, T x) {
  List<T> *tail, *head;
  for (head = tail = xs; xs; xs = xs->next)
    tail = xs;
  if (!head)
    head = cons<T>(x, NULL);
  else
    tail->next = cons<T>(x, NULL);
  return head;
}

/* 
 * Access the last element, O(N) time
 * Suppose the input list isn't empty. 
 */
template<typename T>
T last(List<T>* xs) {
  T x; /* Can be initialized to a special value to indicate empty list err. */
  for (; xs; xs = xs->next)
    x = xs->key;
  return x;
}

/* 
 * Get a sub list contains all elements except the last one, O(N) time.
 * Suppose the input list isn't empty. 
 */
template<typename T>
List<T>* init(List<T>* xs) {
  List<T>* ys = NULL;
  for (; xs->next; xs = xs->next)
    ys = append(ys, xs->key);
  return ys;
}

/* Testing purpose only*/
template<typename T>
void print_lst(List<T>* xs) {
  for(; xs; xs = xs->next)
    cout<<xs->key<<", ";
  cout<<"\n";
}

int main(int, char**) {
  int i, n;
  List<int>* lst = cons(1, cons(2, cons<int>(3, NULL)));
  printf("length of empty = %d, len([1, 2, 3]) = %d\n", length<int>(0), length(lst));
  lst = append(lst, 4);
  printf("append 4 ==>");
  print_lst(lst);
  n = length(lst);
  for(i = 0; i < n; ++i)
    printf("lst[%d] = %d%s", i, getAt(lst, i), i == n-1 ? "\n" : ", ");
  printf("last(lst) = %d, init(lst) = ", last(lst));
  print_lst(init(lst));
  delete lst;
}

#include <iostream>
#include <cstdlib>

template<typename T>
struct List {
  T key;
  List* next;

  List() : next(0) {}
  ~List() { delete next; }
};

template<typename T>
T first(List<T> *xs) { return xs->key; }

template<typename T>
List<T>* rest(List<T>* xs) { return xs->next; }

template<typename T>
List<T>* cons(T x, List<T>* xs) {
  List<T>* lst = new List<T>;
  lst->key = x;
  lst->next = xs;
  return lst;
}


template<typename T>
int length(List<T>* xs) {
  int n = 0;
  for (; xs; ++n, xs = xs->next);
  return n;
}

int main(int, char**) {
  List<int>* lst = cons(1, cons(2, cons<int>(3, NULL)));
  printf("length of empty = %d, len([1, 2, 3]) = %d\n", length<int>(0), length(lst));
  delete lst;
}

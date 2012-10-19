#include <iostream>

template<typename T>
struct List {
  T key;
  List* next;
};

template<typename T>
T first(List<T> *xs) { return xs->key; }

template<typename T>
List<T>* rest(List<T>* xs) { return xs->next; }

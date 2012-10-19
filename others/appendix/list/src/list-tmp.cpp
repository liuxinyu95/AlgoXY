#include <iostream>

struct Empty;

template<n, T> struct List {
  static const int first = n;
  typedef T rest;
};

#include <iostream>
#include <cstdlib>

using namespace std;

struct Empty;

template<int n, typename T> struct List {
  static const int first = n;
  typedef T rest;
};

template<typename L> struct Length{
  static const int value = 1 + Length<typename L::rest>::value;
};

template<> struct Length<Empty> {
  static const int value = 0;
};


int main(int, char**) {
  typedef List<1, List<2, List<3, Empty> > > Lst;
  std::cout<<Length<Empty>::value<<"\n"<<Length<Lst>::value<<"\n";
}

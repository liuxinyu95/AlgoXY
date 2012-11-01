/*
 * list-tmp.cpp
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

using namespace std;

struct Empty;

template<int x, typename T> struct List {
  static const int first = x;
  typedef T rest;
};

/* Auxiliaries */
template<typename L> struct Print {
  static void apply() {
    std::cout<<L::first<<" ";
    Print<typename L::rest>::apply();
  }
};

template<> struct Print<Empty> {
  static void apply() { std::cout<<"\n"; }
};

/* Length */

template<typename L> struct Length {
  static const int value = 1 + Length<typename L::rest>::value;
};

template<> struct Length<Empty> {
  static const int value = 0;
};

/* Get at */

template<typename L, int i> struct GetAt {
  static const int value = GetAt<typename L::rest, i-1>::value;
};

template<typename L> struct GetAt<L, 0> {
  static const int value = L::first;
};

/* Appending */

template<typename L, int x> struct Append {
  typedef List<L::first, typename Append<typename L::rest, x>::result> result;
};

template<int x> struct Append<Empty, x> {
  typedef List<x, Empty> result;
};

/* Last */

template<typename L> struct Last {
  static const int value = Last<typename L::rest>::value;
};

template<int x> struct Last<List<x, Empty> > {
  static const int value = x;
};

/* Init */

template<typename L> struct Init {
  typedef List<L::first, typename Init<typename L::rest>::result> result;
};

template<int x> struct Init<List<x, Empty> > {
  typedef Empty result;
};

/* Drop */
template<int n, typename L> struct Drop {
  typedef typename Drop<n-1, typename L::rest>::result result;
};

template<typename L> struct Drop<0, L> {
  typedef L result;
};

/* index from right*/

template<typename L, int i> struct GetAtR {
  template<typename A, typename B> struct Get {
    static const int value = Get<typename A::rest, typename B::rest>::value;
  };

  template<typename A, int x> struct Get<A, List<x, Empty> > {
    static const int value = A::first;
  };

  static const int value = Get<L, typename Drop<i, L>::result>::value;
};

int main(int, char**) {
  typedef List<1, List<2, List<3, Empty> > > Lst;
  std::cout<<Length<Empty>::value<<"\n"<<Length<Lst>::value<<"\n";
  Print<Lst>::apply();
  Print<Append<Lst, 4>::result>::apply();
  std::cout<<"Lst ! 0 = "<<GetAt<Lst, 0>::value<<", "
           <<"Lst ! 2 = "<<GetAt<Lst, 2>::value<<"\n";
  std::cout<<"last(Lst) = "<<Last<Lst>::value<<", init(Lst) = ";
  Print<Init<Lst>::result>::apply();
  std::cout<<"reverse(Lst) ! 1 = "<<GetAtR<Lst, 1>::value<<"\n";
}

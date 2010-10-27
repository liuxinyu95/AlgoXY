//     bheap.hpp, Implicit Binary Heap by Array in C++
//     Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

//     This program is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.

//     This program is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.

//     You should have received a copy of the GNU General Public License
//     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef _B_HEAP_
#define _B_HEAP_

#include <iostream>
#include <sstream>
#include <string>
#include <sstream>    //std::ostringstream
#include <numeric>    //std::accumulate
#include <functional> //std::ptr_fun
#include <vector>     //random access needed.
#include <iterator>   //std::insert_iterator

template<class T> struct MinHeap: public std::less<T>{};
template<class T> struct MaxHeap: public std::greater<T>{};

// auxiliary functions

// T must support bit-shifting
template<typename T>
T parent(T i){ return (i+1)>>1-1; }

template<typename T>
T left(T i){ return (i<<1)+1; }

template<typename T>
T right(T i){ return (i+1)<<1; }

// Heapify with range
template<typename Array, typename LessOp>
void heapify(Array& a, unsigned int i, unsigned int n, LessOp lt){
  while(true){
    unsigned int l=left(i);
    unsigned int r=right(i);
    unsigned int smallest=i;
    if(l < n && lt(a[l], a[i]))
      smallest = l;
    if(r < n && lt(a[r], a[smallest]))
      smallest = r;
    if(smallest != i){
      std::swap(a[i], a[smallest]);
      i = smallest;
    }
    else
      break;
  }
}

// build heap
// the last non-leaf node is:
// left(i)>=n-1; ==> i>=(n-2)/2
template<typename Array, typename LessOp>
void build_heap(Array& a, unsigned int n, LessOp lt){
  for(unsigned int i = (n-1)>>1; i>=0; --i)
    heapify(a, i, n, lt);
}

/*
template<class T>
T* insert_key(T* t, typename T::key_type x){
  return insert(t, x);
}

template<class Iterator, class T>
T* list_to_btree(Iterator first, Iterator last, T* t){
  return std::accumulate(first, last, t,
                         std::ptr_fun(insert_key<T>));
}
*/

template<typename Iter>
void print_range(Iter first, Iter last){
  for(; first!=last; ++first)
    std::cout<<*first<<", ";
  std::cout<<"\n";
}

class BHeapTest{
public:
  BHeapTest(){
    std::cout<<"Implicit Binary Heap by Array testing\n";
  }

  void run(){
    test_heapify();
    test_build_heap();
  }

private:
  void test_heapify(){
    //test c-array
    int a[] = {16, 4, 10, 14, 7, 9, 3, 2, 8, 1};
    const unsigned int n = sizeof(a)/sizeof(a[0]);
    int x[n];
    std::copy(a, a+n, x);
    heapify(x, 1, n, MaxHeap<int>());
    print_range(x, x+n);
    int r[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    assert(std::equal(r, r+n, x));
    //test random access container
    std::vector<short> y;
    std::copy(a, a+n, std::insert_iterator<std::vector<short> >(y, y.end()));
    heapify(y, 1, n, MaxHeap<short>());
    print_range(y.begin(), y.end());
    assert(std::equal(r, r+n, y.begin()));
  }

  void test_build_heap(){
    //
  }

  void test_delete(){
  }
};

#endif //_B_TREE_

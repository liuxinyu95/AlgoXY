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

// template meta programming tricks
// handle c-array and STL container
template<class T> struct ValueType{
  typedef typename T::value_type Result;
};

template<class T> struct ValueType<T*>{
  typedef T Result; // c-pointer type
};

template<class T, unsigned int n> struct ValueType<T[n]>{
  typedef T Result; // c-array type
};

// auxiliary functions

// T must support bit-shifting
template<class T>
T parent(T i){ return ((i+1)>>1)-1; }

template<class T>
T left(T i){ return (i<<1)+1; }

template<class T>
T right(T i){ return (i+1)<<1; }

// Heapify with range
//
// Liu Xinyu: in STL, Random access iterator and distance 
// of the iterators are used to define the function. Although it's more
// generic, I would rather like to use index of array instead 
// because the algorithm itself looks clear.
//
template<class Array, class LessOp>
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
template<class Array, class LessOp>
void build_heap(Array& a, unsigned int n, LessOp lt){
  unsigned int i = (n-1)>>1;
  while(true){
    heapify(a, i, n, lt);
    if(i==0) break; // this is a trick: unsigned int always >=0
    --i;
  }
}

template<class T>
typename ValueType<T>::Result heap_top(T a){ return a[0]; }

template<class T, class LessOp>
typename ValueType<T>::Result heap_pop(T& a, unsigned int& n, LessOp lt){
  typename ValueType<T>::Result top = heap_top(a);
  a[0] = a[n-1];
  heapify(a, 0, --n, lt);
  return top;
}

// Sort by performing n pop
template<class Array, class LessOp>
std::vector<typename ValueType<Array>::Result > 
heap_sort_slow(Array& a, unsigned int n, LessOp lt){
  std::vector<typename ValueType<Array>::Result > res;
  build_heap(a, n, lt);
  while(n>0)
    res.push_back(heap_pop(a, n, lt));
  return res;
}

// Robert W. Floyd method, inplace fast
template<class Array, class GreaterOp>
void heap_sort(Array& a, unsigned int n, GreaterOp gt){
  build_heap(a, n, gt);
  for(; n>1; --n){
    std::swap(a[0], a[n-1]);
    heapify(a, 0, n-1, gt);
  }
}

template<class Array, class LessOp>
void heap_fix(Array& a, unsigned int i, LessOp lt){
  while(i>0 && lt(a[i], a[parent(i)])){
    std::cout<<"i="<<i<<", parent="<<parent(i)<<"a[parent]="<<a[parent(i)]<<"\n";
    std::swap(a[i], a[parent(i)]);
    i = parent(i);
  }
}

template<class Array, class LessOp>
void heap_decrease_key(Array& a, 
                       unsigned int i, 
                       typename ValueType<Array>::Result key,
                       LessOp lt){
  if(lt(key, a[i])){
    a[i] = key;
    std::cout<<"i="<<i<<", a[i]="<<a[i]<<"\n";
    heap_fix(a, i, lt);
  }
}

// Note, we don't check if a[n] is valid.
// client code should take care of it.
//   example 1:
//     int n = a.size();
//     a.push_back(0);
//     heap_push(a, n, MinHeap<int>());
//   example 2:
//     int a* = new int[11];
//     int n = 10;
//     // initialize a[0]~a[9]
//     heap_push(a, n, MinHeap<int>());
template<class Array, class LessOp>
void heap_push(Array& a, 
               unsigned int& n,
               typename ValueType<Array>::Result key,
               LessOp lt){
  a[n] = key;
  heap_fix(a, n, lt);
  ++n;
}

template<class Array, class LessOp>
std::vector<typename ValueType<Array>::Result> 
top_k(Array& a, unsigned int k, unsigned int n, LessOp lt){
  std::vector<typename ValueType<Array>::Result> res;
  int count = std::min(k, n);
  for(unsigned int i=0; i<count; ++i)
    res.push_back(heap_pop(a, n, lt));
  return res;
}

// helper function to print both STL containers and 
// raw arrays
template<class Iter>
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
    test_heap_sort();
    test_heap_decrease_key();
    //test_heap_push();
    //test_heap_top_k();
  }

private:
  void test_heapify(){
    // CLRS Figure 6.2
    std::cout<<"test heapify\n";
    // test c-array
    const int a[] = {16, 4, 10, 14, 7, 9, 3, 2, 8, 1};
    const unsigned int n = sizeof(a)/sizeof(a[0]);
    int x[n];
    std::copy(a, a+n, x);
    heapify(x, 1, n, MaxHeap<int>());
    print_range(x, x+n);
    int r[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    assert(std::equal(r, r+n, x));
    // test random access container
    std::vector<short> y;
    std::copy(a, a+n, std::insert_iterator<std::vector<short> >(y, y.end()));
    heapify(y, 1, n, MaxHeap<short>());
    print_range(y.begin(), y.end());
    assert(std::equal(r, r+n, y.begin()));
  }

  void test_build_heap(){
    // CLRS Figure 6.3
    std::cout<<"test build heap\n";
    // test c-array
    const int a[] = {4, 1, 3, 2, 16, 9, 10, 14, 8, 7};
    const unsigned int n = sizeof(a)/sizeof(a[0]);
    int x[n];
    std::copy(a, a+n, x);
    build_heap(x, n, MaxHeap<int>());
    print_range(x, x+n);
    const int r[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    assert(std::equal(r, r+n, x));
    // test random access container
    std::vector<short> y;
    std::copy(a, a+n, std::insert_iterator<std::vector<short> >(y, y.end()));
    build_heap(y, n, MaxHeap<short>());
    print_range(y.begin(), y.end());
    assert(std::equal(r, r+n, y.begin()));
    std::cout<<"top of x="<<heap_top(x)<<"\n"
             <<"top of y="<<heap_top(y)<<"\n";
  }

  void test_heap_sort(){
    // CLRS Figure 6.4
    std::cout<<"test heap sort with pop-n method\n";
    const int a[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    const int r[] = {1, 2, 3, 4, 7, 8, 9, 10, 14, 16};
    const unsigned int n = sizeof(a)/sizeof(a[0]);
    int x[n];
    std::copy(a, a+n, x);
    std::vector<int> x1 = heap_sort_slow(x, n, MinHeap<int>());
    print_range(x1.begin(), x1.end());
    assert(std::equal(r, r+n, x1.begin()));

    std::cout<<"test heap sort with Floyd method\n";
    std::vector<short> y;
    std::copy(a, a+n, std::insert_iterator<std::vector<short> >(y, y.end()));
    heap_sort(y, n, MaxHeap<short>());
    print_range(y.begin(), y.end());
    assert(std::equal(r, r+n, y.begin()));
  }

  void test_heap_decrease_key(){
    // CLRS Figure 6.5
    std::cout<<"test heap decrease key\n";
    const int a[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    const unsigned int n = sizeof(a)/sizeof(a[0]);
    int x[n];
    std::copy(a, a+n, x);
    heap_decrease_key(x, 8, 15, MaxHeap<int>());
    print_range(x, x+n);
    const int r[] = {16, 15, 10, 14, 7, 9, 3, 2, 8, 1};
    assert(std::equal(r, r+n, x));
  }
};

#endif //_B_TREE_

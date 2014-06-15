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
#include <cassert>

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

//
// Range, (not Boost::Range), just a simplified version
// [left, right)
template<class RIter> // random access iterator
struct Range{
  typedef typename std::iterator_traits<RIter>::value_type value_type;
  typedef typename std::iterator_traits<RIter>::difference_type size_t;
  typedef typename std::iterator_traits<RIter>::reference  reference;
  typedef RIter iterator;

  Range(RIter left, RIter right):first(left), last(right){}

  reference  operator[](size_t i){ return *(first+i); }
  size_t size() const { return last-first; }

  RIter first;
  RIter last;
};

template<class Iter>
Range<Iter> range(Iter left, Iter right){ return Range<Iter>(left, right); }

template<class Iter>
Range<Iter> range(Iter left, typename Range<Iter>::size_t n){
  return Range<Iter>(left, left+n);
}

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

// Heapify with Range abstraction
template<class R, class LessOp>
void heapify(R a, typename R::size_t i, LessOp lt){
  typename R::size_t l, r, smallest;
  while(true){
    l = left(i);
    r = right(i);
    smallest = i;
    if( l < a.size() && lt(a[l], a[i]))
      smallest = l;
    if( r < a.size() && lt(a[r], a[smallest]))
      smallest = r;
    if( smallest != i){
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
  do {
      heapify(a, i, n, lt);
  } while (i--);
}

template<class RangeType, class LessOp>
void build_heap(RangeType a, LessOp lt){
  typename RangeType::size_t i = (a.size()-1)>>1;
  do {
    heapify(a, i, lt);
  } while (i--);
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

// range is modified after pop, popped element is put to array[last]
template<class R, class LessOp>
typename R::value_type heap_pop(R& a, LessOp lt){
  typename R::value_type top = heap_top(a);
  std::swap(a[0], a[a.size()-1]);
  --a.last;
  heapify_(a, 0, lt);
  return top;
}

// Sort by performing n pop
template<class Iter, class Array, class LessOp>
void heap_sort_slow(Iter res, Array& a, unsigned int n, LessOp lt){
  build_heap(a, n, lt);
  while(n>0)
    *res++=heap_pop(a, n, lt);
}

// Robert W. Floyd method, inplace fast
template<class Array, class GreaterOp>
void heap_sort(Array& a, unsigned int n, GreaterOp gt){
  for(build_heap(a, n, gt); n>1; --n){
    std::swap(a[0], a[n-1]);
    heapify(a, 0, n-1, gt);
  }
}

// Floyd method with Range
template<class R, class GreaterOp>
void heap_sort(R a, GreaterOp gt){
  build_heap(a, gt);
  while(a.size()>1){
    std::swap(a[0], a[a.size()-1]);
    --a.last;
    heapify(a, 0, gt);
  }
}

template<class Array, class LessOp>
void heap_fix(Array& a, unsigned int i, LessOp lt){
  while(i>0 && lt(a[i], a[parent(i)])){
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

template<class R, class LessOp>
void heap_push(R a, typename R::value_type key, LessOp lt){
  *a.last++ = key;
  heap_fix(a, a.size()-1, lt);
}

template<class Iter, class Array, class LessOp>
void heap_top_k(Iter res, unsigned int k,
                Array& a, unsigned int& n, LessOp lt){
  build_heap(a, n, lt);
  unsigned int count = std::min(k, n);
  for(unsigned int i=0; i<count; ++i)
    *res++=heap_pop(a, n, lt);
}

// helper function to print both STL containers and
// raw arrays
template<class Iter>
void print_range(Iter first, Iter last){
  for(; first!=last; ++first)
    std::cout<<*first<<", ";
  std::cout<<"\n";
}

template<class R>
void print_range(R a){
  print_range(a.first, a.last);
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
    test_heap_push();
    test_heap_top_k();
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
    std::vector<short> y(a, a+n);
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
    std::vector<int> y(a, a+n);
    build_heap(y, n, MaxHeap<int>());
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
    int x1[n];
    heap_sort_slow(x1, x, n, MinHeap<int>());
    print_range(x1, x1+n);
    assert(std::equal(r, r+n, x1));

    std::cout<<"test heap sort with Floyd method\n";
    std::vector<int> y(a, a+n);
    heap_sort(y, n, MaxHeap<int>());
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

  void test_heap_push(){
    std::cout<<"test heap push\n";
    const int a[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    unsigned int n = sizeof(a)/sizeof(a[0]);
    std::vector<int> x(a, a+n);
    x.push_back(0);
    heap_push(x, n, 17, MaxHeap<int>()); //n increased
    print_range(x.begin(), x.end());
    const int r[] = {17, 16, 10, 8, 14, 9, 3, 2, 4, 1, 7};
    assert(std::equal(r, r+n, x.begin()));
  }

  void test_heap_top_k(){
    std::cout<<"test top k elements\n";
    const int a[] = {4, 1, 3, 2, 16, 9, 10, 14, 8, 7};
    unsigned int n = sizeof(a)/sizeof(a[0]);
    std::vector<int> x(a, a+n);
    int y[3];
    heap_top_k(y, 3, x, n, MaxHeap<int>());
    print_range(y, y+3);
    const int r[] = {16, 14, 10};
    assert(std::equal(r, r+3, y));
  }
};

class BHeapByRangeTest{
public:
  BHeapByRangeTest(){
    std::cout<<"Implicit Binary Heap by Array with Range abstraction testing\n";
  }

  void run(){
    test_heapify();
    test_build_heap();
    test_heap_sort();
    test_heap_push();
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
    heapify(range(x, n), 1, MaxHeap<int>());
    print_range(x, x+n);
    int r[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    assert(std::equal(r, r+n, x));

    // test random access container
    std::vector<int> y(a, a+n);
    heapify(range(y.begin(), y.end()), 1, MaxHeap<int>());
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
    build_heap(range(x, n), MaxHeap<int>());
    print_range(x, x+n);
    const int r[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    assert(std::equal(r, r+n, x));

    // test random access container
    std::vector<int> y(a, a+n);
    build_heap(range(y.begin(), y.end()), MaxHeap<short>());
    print_range(y.begin(), y.end());
    assert(std::equal(r, r+n, y.begin()));
    std::cout<<"top of x="<<heap_top(x)<<"\n"
             <<"top of y="<<heap_top(y)<<"\n";
  }

  void test_heap_sort(){
    // CLRS Figure 6.4
    const int a[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    const int r[] = {1, 2, 3, 4, 7, 8, 9, 10, 14, 16};
    const unsigned int n = sizeof(a)/sizeof(a[0]);

    std::cout<<"test heap sort with Floyd method\n";
    std::vector<int> y(a, a+n);
    heap_sort(range(y.begin(), y.end()), MaxHeap<int>());
    print_range(y.begin(), y.end());
    assert(std::equal(r, r+n, y.begin()));
  }

  void test_heap_push(){
    std::cout<<"test heap push\n";
    const int a[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    unsigned int n = sizeof(a)/sizeof(a[0]);
    std::vector<int> x(a, a+n);
    x.push_back(0);
    heap_push(range(x.begin(), n), 17, MaxHeap<int>());
    print_range(x.begin(), x.end());
    const int r[] = {17, 16, 10, 8, 14, 9, 3, 2, 4, 1, 7};
    assert(std::equal(r, r+n, x.begin()));
  }
};


#endif //_B_HEAP_

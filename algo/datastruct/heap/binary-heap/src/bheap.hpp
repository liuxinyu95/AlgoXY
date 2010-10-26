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

// auxiliary functions

// T must support bit-shifting
template<typename T>
T parent(T i){ return (i+1)>>1-1; }

template<typename T>
T left(T i){ return (i<<1)+1; }

template<typename T>
T right(T i){ return (i+1)<<1; }

// tricky template meta programming helpers
template<class T>
struct MinHeap{
  typedef std::less<typename T::value_type> Comp;
};

template<class T> struct MinHeap<T*>{
  typedef std::less<T> Comp;
};

template<class T>
struct MaxHeap{
  typedef std::greater<typename T::value_type> Comp;
};

template<class T> struct MaxHeap<T*>{
  typedef std::greater<T> Comp;
};

template<class T>
typename MinHeap<T>::Comp min_heap(T){
  return typename MinHeap<T>::Comp();
}

template<class T>
typename MaxHeap<T>::Comp max_heap(T){
  return typename MaxHeap<T>::Comp();
}

// Heapify with range
template<typename Array, typename Index, typename LessOp>
void heapify(Array& a, Index i, Index n, LessOp lt){
  while(true){
    Index l=left(i);
    Index r=right(i);
    Index smallest=i;
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

class BHeapTest{
public:
  BHeapTest(){
    std::cout<<"Implicit Binary Heap by Array testing\n";
  }

  void run(){
    test_heapify();
    //test_insert();
    //test_delete();
  }

private:
  void test_heapify(){
    int a[] = {16, 4, 10, 14, 7, 9, 3, 2, 8, 1};
    int x[sizeof(a)/sizeof(int)];
    std::copy(a, a+sizeof(a)/sizeof(int), x);
    heapify(x, (unsigned int)1, sizeof(a)/sizeof(int), max_heap(x));
    std::copy(x, x+sizeof(a)/sizeof(int), 
              std::ostream_iterator<int>(std::cout, ", "));
    int r[] = {16, 14, 10, 8, 7, 9, 3, 2, 4, 1};
    assert(std::equal(r, r+sizeof(r)/sizeof(int), x));
  }

  void test_insert(){
  }

  void test_delete(){
  }
};

#endif //_B_TREE_

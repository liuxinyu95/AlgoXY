#include <vector>
#include <iostream>
#include <cstdlib> //for verification purpose only
#include <deque>
#include <cassert>
#include <iterator>

/*
 * Paired-array list
 */

using namespace std;

#define BIG_RAND() (rand() % 1000)

template<typename Key>
struct List {
  int n, m;
  vector<Key> front;
  vector<Key> rear;

  List() : n(0), m(0) {}
  int size() { return n + m; }

  void print() {
    printf("n=%d, m=%d\n", n, m);
    cout<<"front:";
    copy(front.begin(), front.end(), ostream_iterator<Key>(cout, ", "));
    cout<<"\nrear: ";
    copy(rear.begin(), rear.end(), ostream_iterator<Key>(cout, ", "));
    cout<<"\n";
  }
};

/* Insertion and appending */
template<typename Key>
void insert(List<Key>& xs, Key x) {
  ++xs.n;
  xs.front.push_back(x);
}

template<typename Key>
void append(List<Key>& xs, Key x) {
  ++xs.m;
  xs.rear.push_back(x);
}

/* 
 * Removing and balancing 
 * Skip error handling for illustratoin purpose.
 */

template<typename Key>
void balance(List<Key>& xs) {
  if(xs.n == 0) {
    back_insert_iterator<vector<Key> > i(xs.front);
    reverse_copy(xs.rear.begin(), xs.rear.begin() + (xs.m >> 1), i);
    xs.rear.erase(xs.rear.begin(), xs.rear.begin() +(xs.m >> 1));
    xs.n = (xs.m >> 1);
    xs.m -= xs.n;
  }
  else if(xs.m == 0) {
    swap(xs.front, xs.rear);
    swap(xs.n, xs.m);
    balance(xs);
    swap(xs.front, xs.rear);
    swap(xs.n, xs.m);
  }
}

template<typename Key>
void remove_head(List<Key>& xs) {
  balance(xs);
  if(xs.front.empty())
    remove_tail(xs); //remove the singleton elem in rear
  else {
    xs.front.pop_back();
    --xs.n;
  }
}

template<typename Key>
void remove_tail(List<Key>& xs) {
  balance(xs);
  if(xs.rear.empty())
    remove_head(xs); //remove the singleton elem in front
  else {
    xs.rear.pop_back();
    --xs.m;
  }
}

/* Random accessing, skip the out-of-bound error handling*/
template<typename Key>
Key get(List<Key>& xs, int i) {
  if( i < xs.n )
    return xs.front[xs.n-i-1];
  else
    return xs.rear[i-xs.n];
}

template<typename Key>
void set(List<Key>& xs, int i, Key x) {
  if( i < xs.n )
    xs.front[xs.n-i-1] = x;
  else
    xs.rear[i-xs.n] = x;
}

/* Verification */
template<class Key>
void assert_eq(deque<Key> xs, List<Key> ys) {
  typename deque<Key>::iterator i;
  int j;
  for(i = xs.begin(), j = 0; i!=xs.end(); ++i, ++j)
    assert(*i == get(ys, j));
}

template<class Key>
void print_list(List<Key> xs) {
  for(int i = 0; i < xs.size(); ++i)
    printf( i==xs.size() -1 ? "%d\n" : "%d, ", get(xs, i));
}

void print_sample(deque<int>& xs, List<int>& ys) {
  cout<<"\ndeque:\n";
  copy(xs.begin(), xs.end(), ostream_iterator<int>(cout, ", "));
  cout<<"\npalist:\n";
  print_list(ys);
}

int sample(deque<int>& xs, List<int>& ys) {
  int n = BIG_RAND() + 1;
  for(int i=0; i<n; ++i) {
    int x = BIG_RAND();
    if(BIG_RAND() % 2) {
      xs.push_back(x);
      append(ys, x);
    }
    else {
      xs.push_front(x);
      insert(ys, x);
    }
  }
  return n;
}

void test_insert() {
  for(int m=0; m<100; ++m) {
    deque<int> xs;
    List<int> ys;
    sample(xs, ys);
    assert_eq(xs, ys);
  }
}

void test_remove() {
  for(int m=0; m<100; ++m) {
    deque<int> xs;
    List<int> ys;
    int n = sample(xs, ys);
    int c = rand() % n + 1;
    for(int i=0; i<c; ++i) {
      if(BIG_RAND() % 2) {
        xs.pop_back();
        remove_tail(ys);
      }
      else{
        xs.pop_front();
        remove_head(ys);
      }
    }
    assert_eq(xs, ys);
  }
}

int main(int, char**) {
  test_insert();
  test_remove();
  return 0;
}

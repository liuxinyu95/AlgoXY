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

#define BIG_RAND() (rand() % 20)

template<typename Key>
struct List {
  int n, m;
  vector<Key> front;
  vector<Key> rear;

  List() : n(0), m(0) {}

  int size() { return n + m; }
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

/* Removing and balancing */

template<typename Key>
void remove_head(List<Key>& xs) {
  //TODO: balancing
  xs.front.pop_back();
}

template<typename Key>
void remove_tail(List<Key>& xs) {
  //TODO: balancing
  xs.rear.pop_back();
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

void test_insert() {
  for(int m=0; m<100; ++m) {
    int n = BIG_RAND();
    deque<int> xs;
    List<int> ys;
    for(int i=0; i<n; ++i) {
      int x = BIG_RAND();
      bool dir = BIG_RAND() % 2 == 0;
      if(dir) {
        xs.push_back(x);
        append(ys, x);
      }
      else {
        xs.push_front(x);
        insert(ys, x);
      }
    }
    //copy(xs.begin(), xs.end(), ostream_iterator<int>(cout, ", "));
    //print_list(ys);
    assert_eq(xs, ys);
  }
}

int main(int, char**) {
  test_insert();
  return 0;
}

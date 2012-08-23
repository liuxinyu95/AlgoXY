#include <vector>
#include <iostream>

using namespace std;

template<typename Key>
struct List {
  int n, m;
  vector<Key> front;
  vector<Key> end;
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
    return xs.front[n-i-1];
  else
    return xs.rear[i-n];
}

template<typename Key>
set(List<Key>& xs, int i, Key x) {
  if( i < xs.n )
    xs.front[n-i-1] = x;
  else
    xs.rear[i-n] = x;
}

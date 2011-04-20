//     intpatricia.hpp, Integer based Patricia
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

#ifndef _INT_PATRICIA
#define _INT_PATRICIA
#include <iostream>
#include <string>
#include <sstream>
#include <iterator>
#include <functional>
#include <numeric> //for std::accumulate

int maskbit(int x, int mask){
  return x & (~(mask-1));
}

bool zero(int x, int mask){
  return (x & (mask>>1)) == 0;
}

//the longest common prefix, return the mask
int lcp(int& p, int p1, int p2){
  int diff = p1^p2;
  int mask = 1;
  while(diff){
    diff>>=1;
    mask<<=1;
  }
  p = maskbit(p1, mask);
  return mask;
}

template<class T>
struct IntPatricia{
  IntPatricia(int k=0, T v=T()): 
    key(k), value(v), prefix(k), mask(1), left(0), right(0){}

  ~IntPatricia(){
    delete left;
    delete right;
  }

  bool is_leaf(){
    return left == 0 && right == 0;
  }

  bool match(int x){
    return (!is_leaf()) && (maskbit(x, mask) == prefix);
  }

  void replace_child(IntPatricia<T>* x, IntPatricia<T>* y){
    if(left == x)
      left = y;
    else
      right = y;
  }

  void set_children(IntPatricia<T>* l, IntPatricia<T>* r){
    left = l;
    right = r;
  }

  int key;
  T value;
  int prefix;
  int mask;
  IntPatricia* left;
  IntPatricia* right;
};

template<class T>
IntPatricia<T>* branch(IntPatricia<T>* t1, IntPatricia<T>* t2){
  IntPatricia<T>* t = new IntPatricia<T>();
  t->mask = lcp(t->prefix, t1->prefix, t2->prefix);
  if(zero(t1->prefix, t->mask))
    t->set_children(t1, t2);
  else
    t->set_children(t2, t1);
  return t;
}

template<class T>
IntPatricia<T>* insert(IntPatricia<T>* t, int key, T value=T()){
  if(!t)
    return new IntPatricia<T>(key, value);

  IntPatricia<T>* node = t;
  IntPatricia<T>* parent(0);

  while( node->is_leaf()==false && node->match(key) ){
    parent = node;
    if(zero(key, node->mask))
      node = node->left;
    else
      node = node->right;
  }

  if(node->is_leaf() && key == node->key)
    node->value = value;
  else{
    IntPatricia<T>* p = branch(node, new IntPatricia<T>(key, value));
    if(!parent)
      return p;
    parent->replace_child(node, p);
  }
  return t;
}

template<class T>
T lookup(IntPatricia<T>* t, int key){
  if(!t)
    return T(); //or throw exception

  while( (!t->is_leaf()) && t->match(key)){
    if(zero(key, t->mask))
      t = t->left;
    else
      t = t->right;
  }
  if(t->is_leaf() && t->key == key)
    return t->value;
  else
    return T(); //or throw exception
}

template<class T>
std::string patricia_to_str(IntPatricia<T>* t){
  if(!t)
    return "";
  std::stringstream s;
  if(t->is_leaf()){
    s<<t->key;
    if(t->value != T())
      s<<":"<<t->value;
  }
  else
    s<<"["<<t->prefix<<"@"<<t->mask<<"]("
     <<patricia_to_str(t->left)<<","
     <<patricia_to_str(t->right)<<")";
  return s.str();
}

// C++ version of foldl is std::accumulate
// here are 2 helper functions for foldl
template<class T>
IntPatricia<T>* insert_key(IntPatricia<T>* t, int key){
  return insert(t, key);
}

template<class T, class Pair>
IntPatricia<T>* insert_pair(IntPatricia<T>* t, Pair p){
  return insert(t, p.first, p.second);
}

class IntPatriciaTest{
public:
  IntPatriciaTest():ti(0), tc(0){}

  ~IntPatriciaTest(){
    delete ti;
    delete tc;
  }

  void run(){
    std::cout<<"\ntest int patricia\n";
    test_patricia_insert();
    test_patricia_lookup();
  }

private:
  void test_patricia_insert(){
    const int lst[] = {6, 7};
    ti = std::accumulate(lst, lst+sizeof(lst)/sizeof(int), ti, std::ptr_fun(insert_key<int>));
    std::copy(lst, lst+sizeof(lst)/sizeof(int), std::ostream_iterator<int>(std::cout, ", "));
    std::cout<<"==>"<<patricia_to_str(ti)<<"\n";

    const int keys[] = {1, 4, 5};
    const char vals[] = "xyz";
    for(unsigned int i=0; i<sizeof(keys)/sizeof(int); ++i)
      tc = insert(tc, keys[i], vals[i]);
    std::copy(keys, keys+sizeof(keys)/sizeof(int),
	      std::ostream_iterator<int>(std::cout, ", "));
    std::cout<<"==>"<<patricia_to_str(tc);
  }

  void test_patricia_lookup(){
    std::cout<<"\nlook up 4: "<<lookup(tc, 4)
             <<"\nlook up 0: "<<lookup(tc, 0)<<"\n";
  }

  IntPatricia<int>* ti;
  IntPatricia<char>* tc;
};
#endif //_INT_PATRICIA

#ifndef _INT_PATRICIA
#define _INT_PATRICIA
#include <iostream>
#include <string>
#include <sstream>
#include <functional>

bool maskbit(int x, int mask){
  return x & (~(mask-1));
}

template<class T>
struct IntPatricia{
  IntPatricia(int k=0, T v=T()): 
    key(k), value(v),prefix(), mask(), left(0), right(0){}

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

  int key;
  T value;
  int prefix;
  int mask;
  IntPatricia* left;
  IntPatricia* right;
};

template<class T>
IntPatricia<T>* insert(IntPatricia<T>* t, int key, T value=T()){
  if(!t)
    return IntPatricia<T>(key, value);

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
    IntPatricia<T>* p = branch(node, IntPatricia<T>(key, value));
    if(!parent)
      return p;
    parent->replace_child(node, p);
  }
  return t;
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

//C++ version of foldl is std::accumulate
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
    test_patricia_insert();
  }

private:
  void test_patricia_insert(){
    const int lst[] = {6};
    std::list<int> l(lst, lst+sizeof(lst)/sizeof(int));
    ti = std::accumulate(l.begin(), l.end(), ti, std::ptr_fun(insert_key<int>));

    const int keys[] = {1, 4, 5};
    const char vals[] = "xyz";
  }

  IntPatricia<int>* ti;
  IntPatricia<char>* tc;
};
#endif //_INT_PATRICIA

//     btree.hpp, B-tree implemented in C++
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

#ifndef _B_TREE_
#define _B_TREE_

#include <iostream>
#include <sstream>
#include <string>
#include <sstream>    //std::ostringstream
#include <numeric>    //std::accumulate
#include <functional> //std::ptr_fun
#include <vector>     //random access needed.

// t: minimum degree of B-tree
template<class K, int t>
struct BTree{
  typedef K key_type;
  typedef std::vector<K> Keys;
  typedef std::vector<BTree*> Children;

  BTree(){}

  ~BTree(){
    for(typename Children::iterator it=children.begin();
        it!=children.end(); ++it)
      delete (*it);
  }

  //  k0, k1, ... ki, k{i+1}, ...
  //c0, c1, ..., ci, c{i+1}, ...
  // perform split on ci
  //
  void split_child(int i){
    BTree<K, t>* x = children[i];
    BTree<K, t>* y = new BTree<K, t>();
    keys.insert(keys.begin()+i, x->keys[t-1]);
    children.insert(children.begin()+i+1, y);
    y->keys = Keys(x->keys.begin()+t, x->keys.end());
    x->keys = Keys(x->keys.begin(), x->keys.begin()+t-1);
    if(!y->leaf()){
      y->children = Children(x->children.begin()+t, x->children.end());
      x->children = Children(x->children.begin(), x->children.begin()+t);
    }
  }

  bool full(){ return keys.size() == 2*t-1; }

  bool leaf(){
    return (!keys.empty()) && children.empty();
  }

  Keys keys;
  Children children; 
};

template<class K, int t>
BTree<K, t>* insert(BTree<K, t>* tr, K key){
  BTree<K, t>* root(tr);
  if(root->full()){
    BTree<K, t>* s = new BTree<K, t>();
    s->children.push_back(root);
    s->split_child(0);
    root = s;
  }
  return insert_nonfull(root, key);
}

template<class Coll>
void orderred_insert(Coll& coll, typename Coll::value_type x){
  typename Coll::iterator it = coll.begin();
  while(it != coll.end() && *it < x)
    ++it;
  coll.insert(it, x);
}

template<class K, int t>
BTree<K, t>* insert_nonfull(BTree<K, t>* tr, K key){
  typedef typename BTree<K, t>::Keys Keys;
  typedef typename BTree<K, t>::Children Children;

  BTree<K, t>* root(tr);
  while(!tr->leaf()){
    unsigned int i=0;
    while(i < tr->keys.size() && tr->keys[i] < key)
      ++i;
    if(tr->children[i]->full()){
      tr->split_child(i);
      if(key > tr->keys[i])
        ++i;
    }
    tr = tr->children[i];
  }
  orderred_insert(tr->keys, key);
  return root;
}

template<class T>
T* insert_key(T* t, typename T::key_type x){
  return insert(t, x);
}

template<class Iterator, class T>
T* list_to_btree(Iterator first, Iterator last, T* t){
  return std::accumulate(first, last, t,
                         std::ptr_fun(insert_key<T>));
}

//generic auxiliary functions
template<class T>
std::string to_str(T x){
  std::ostringstream s;
  s<<x;
  return s.str();
}

template<class T>
std::string btree_to_str(T* tr){
  typename T::Keys::iterator k;
  typename T::Children::iterator c;

  std::ostringstream s;
  s<<"(";
  if(tr->leaf()){
    k=tr->keys.begin();
    s<<*k++;
    for(; k!=tr->keys.end(); ++k)
      s<<", "<<*k;
  }
  else{
    for(k=tr->keys.begin(), c=tr->children.begin();
        k!=tr->keys.end(); ++k, ++c)
      s<<btree_to_str(*c)<<", "<<*k<<", ";
    s<<btree_to_str(*c);
  }
  s<<")";
  return s.str();
}

class BTreeTest{
public:
  BTreeTest(){
    std::cout<<"B-tree testing\n";
  }

  void run(){
    test_insert();
  }
private:
  void test_insert(){
    const char* ss[] = {"G", "M", "P", "X", "A", "C", "D", "E", "J", "K", \
                        "N", "O", "R", "S", "T", "U", "V", "Y", "Z"};
    BTree<std::string, 2>* tr234=list_to_btree(ss, ss+sizeof(ss)/sizeof(char*),
                                               new BTree<std::string, 2>);
    std::cout<<"2-3-4 tree of ";
    std::copy(ss, ss+sizeof(ss)/sizeof(char*), 
              std::ostream_iterator<std::string>(std::cout, ", "));
    std::cout<<"\n"<<btree_to_str(tr234)<<"\n";
  }
};

#endif //_B_TREE_

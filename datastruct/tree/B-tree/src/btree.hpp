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
#include <iterator>   //std::insert_iterator

//generic auxilary functions
// x ++ y in Haskell
template<class Coll>
void concat(Coll& x, Coll& y){
  std::copy(y.begin(), y.end(), 
            std::insert_iterator<Coll>(x, x.end()));
}

//debug only
//
template<class Coll>
void print_ss(const Coll& ss){
  std::copy(ss.begin(), ss.end(),
            std::ostream_iterator<typename Coll::value_type>(std::cout, ", "));
  std::cout<<"\n";
}

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
    if(!x->leaf()){
      y->children = Children(x->children.begin()+t, x->children.end());
      x->children = Children(x->children.begin(), x->children.begin()+t);
    }
  }

  // merge children[i], keys[i], and children[i+1] to one node
  void merge_children(int i){
    BTree<K, t>* x = children[i];
    BTree<K, t>* y = children[i+1];
    x->keys.push_back(keys[i]);
    concat(x->keys, y->keys);
    concat(x->children, y->children);
    keys.erase(keys.begin()+i);
    children.erase(children.begin()+i+1);
    y->children.clear();
    delete y;
  }

  key_type replace_key(int i, key_type key){
    keys[i]=key;
    return key;
  }

  bool full(){ return keys.size() == 2*t-1; }

  bool can_remove(){ return keys.size() >=t; }

  bool leaf(){
    return children.empty();
  }

  Keys keys;
  Children children; 
};

template<class T>
T* insert(T* tr, typename T::key_type key){
  T* root(tr);
  if(root->full()){
    T* s = new T;
    s->children.push_back(root);
    s->split_child(0);
    root = s;
  }
  return insert_nonfull(root, key);
}

template<class Coll>
void ordered_insert(Coll& coll, typename Coll::value_type x){
  typename Coll::iterator it = coll.begin();
  while(it != coll.end() && *it < x)
    ++it;
  coll.insert(it, x);
}

template<class T>
T* insert_nonfull(T* tr, typename T::key_type key){
  typedef typename T::Keys Keys;
  typedef typename T::Children Children;

  T* root(tr);
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
  ordered_insert(tr->keys, key);
  return root;
}

template<class T>
T* del(T* tr, typename T::key_type key){
  T* root(tr);
  while(!tr->leaf()){
    unsigned int i = 0;
    bool located(false);
    while(i < tr->keys.size()){
      if(key == tr->keys[i]){
        located = true;
        if(tr->children[i]->can_remove()){ //case 2a
          key = tr->replace_key(i, tr->children[i]->keys.back());
          tr->children[i]->keys.pop_back();
          tr = tr->children[i];
        }
        else if(tr->children[i+1]->can_remove()){ //case 2b
          key = tr->replace_key(i, tr->children[i+1]->keys.front());
          tr->children[i+1]->keys.erase(tr->children[i+1]->keys.begin());
          tr = tr->children[i+1];
        }
        else{ //case 2c
          tr->merge_children(i);
          if(tr->keys.empty()){ //shrinks height
            T* temp = tr->children[0];
            tr->children.clear();
            delete tr;
            tr = temp;
          }
        }
        break;
      }
      else if(key > tr->keys[i])
        i++;
      else
        break;
    }
    if(located)
      continue;
    if(!tr->children[i]->can_remove()){ //case 3
      if(i>0 && tr->children[i-1]->can_remove()){ 
        // case 3a: left sibling
        tr->children[i]->keys.insert(tr->children[i]->keys.begin(),
                                     tr->keys[i-1]);
        tr->keys[i-1] = tr->children[i-1]->keys.back();
        tr->children[i-1]->keys.pop_back();
        if(!tr->children[i]->leaf()){
          tr->children[i]->children.insert(tr->children[i]->children.begin(),
                                           tr->children[i-1]->children.back());
          tr->children[i-1]->children.pop_back();
        }
      }
      else if(i<tr->children.size() && tr->children[i+1]->can_remove()){
        // case 3a: right sibling
        tr->children[i]->keys.push_back(tr->keys[i]);
        tr->keys[i] = tr->children[i+1]->keys.front();
        tr->children[i+1]->keys.erase(tr->children[i+1]->keys.begin());
        if(!tr->children[i]->leaf()){
          tr->children[i]->children.push_back(tr->children[i+1]->children.front());
          tr->children[i+1]->children.erase(tr->children[i+1]->children.begin());
        }
      }
      else{
        if(i>0)
          tr->merge_children(i-1);
        else
          tr->merge_children(i);
      }
    }
    tr = tr->children[i];
  }
  tr->keys.erase(remove(tr->keys.begin(), tr->keys.end(), key), 
                 tr->keys.end());
  if(root->keys.empty()){ //shrinks height
    T* temp = root->children[0];
    root->children.clear();
    delete root;
    root = temp;
  }
  return root;
}

template<class T>
std::pair<T*, unsigned int> search(T* t, typename T::key_type k){
  for(;;){
    unsigned int i(0);
    for(; i < t->keys.size() && k > t->keys[i]; ++i);
    if(i < t->keys.size() && k == t->keys[i])
      return std::make_pair(t, i);
    if(t->leaf())
      break;
    t = t->children[i];
  }
  return std::make_pair((T*)0, 0); //not found
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

// quick and dirty helper function to change a B-tree description
// string to a B-tree.
template<class T>
T* parse(std::string::iterator& first, std::string::iterator last){
  T* tr = new T;
  ++first; //'('
  while(first!=last){
    if(*first=='('){ //child
      tr->children.push_back(parse<T>(first, last));
    }
    else if(*first == ',' || *first == ' ')
      ++first; //skip deliminator
    else if(*first == ')'){
      ++first;
      return tr;
    }
    else{ //key
      typename T::key_type key;
      while(*first!=',' && *first!=')')
        key+=*first++;
      tr->keys.push_back(key);
    }
  }
  //should never run here
  return 0;
}

template<class T>
T* str_to_btree(std::string s){
  std::string::iterator first(s.begin());
  return parse<T>(first, s.end());
}

class BTreeTest{
public:
  BTreeTest(){
    std::cout<<"B-tree testing\n";
  }

  void run(){
    test_insert();
    test_delete();
    test_search();
    //__test_insert_verbose();
    //__test_ordered_insert();
    //__test_parse();
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
    delete tr234;

    BTree<std::string, 3>* tr = list_to_btree(ss, ss+sizeof(ss)/sizeof(char*),
                                              new BTree<std::string, 3>);
    std::cout<<"B-tree with t=3 of ";
    std::copy(ss, ss+sizeof(ss)/sizeof(char*), 
              std::ostream_iterator<std::string>(std::cout, ", "));
    std::cout<<"\n"<<btree_to_str(tr)<<"\n";
    delete tr;
  }

  void __test_insert_verbose(){
    const char* ss[] = {"G", "M", "P", "X", "A", "C", "D", "E", "J", "K", \
                        "N", "O", "R", "S", "T", "U", "V", "Y", "Z"};
    const int len = sizeof(ss)/sizeof(char*);
    for(int i=1; i<len; ++i){
      BTree<std::string, 2>* tr234=list_to_btree(ss, ss+i,
                                                 new BTree<std::string, 2>);
      std::cout<<"2-3-4 tree of ";
      std::copy(ss, ss+i, 
                std::ostream_iterator<std::string>(std::cout, ", "));
      std::cout<<"\n"<<btree_to_str(tr234)<<"\n";
      delete tr234;
    }
  }

  void __test_ordered_insert(){
    std::cout<<"test ordered insert...\n";
    std::string s("beh");
    std::cout<<"s="<<s<<"\n";
    ordered_insert(s, 'a');
    print_ss(s);
    ordered_insert(s, 'c');
    print_ss(s);
    ordered_insert(s, 'x');
    print_ss(s);
  }

  void test_delete(){
    std::cout<<"test delete...\n";
    const char* s="(((A, B), C, (D, E, F), G, (J, K, L), M, (N, O)), "
                  "P, ((Q, R, S), T, (U, V), X, (Y, Z)))";
    typedef BTree<std::string, 3> BTr;
    BTr* tr = str_to_btree<BTr>(s);
    std::cout<<"before delete:\n"<<btree_to_str(tr)<<"\n";
    const char* ks[] = {"F", "M", "G", "D", "B", "U"};
    for(unsigned int i=0; i<sizeof(ks)/sizeof(char*); ++i)
      tr=__test_del__(tr, ks[i]);
    delete tr;
  }

  template<class T>
  T* __test_del__(T* tr, typename T::key_type key){
    std::cout<<"delete "<<key<<"==>\n";
    tr = del(tr, key);
    std::cout<<btree_to_str(tr)<<"\n";
    return tr;
  }

  void test_search(){
    std::cout<<"test search...\n";
    const char* ss[] = {"G", "M", "P", "X", "A", "C", "D", "E", "J", "K", \
                        "N", "O", "R", "S", "T", "U", "V", "Y", "Z"};
    BTree<std::string, 3>* tr = list_to_btree(ss, ss+sizeof(ss)/sizeof(char*),
                                              new BTree<std::string, 3>);
    std::cout<<"\n"<<btree_to_str(tr)<<"\n";
    for(unsigned int i=0; i<sizeof(ss)/sizeof(char*); ++i)
      __test_search(tr, ss[i]);
    __test_search(tr, "W");
    delete tr;
  }

  template<class T>
  void __test_search(T* t, typename T::key_type k){
    std::pair<T*, unsigned int> res = search(t, k);
    if(res.first)
      std::cout<<"found "<<res.first->keys[res.second]<<"\n";
    else
      std::cout<<"not found "<<k<<"\n";
  }

  void __test_parse(){
    std::cout<<"test parsing...\n";
    const char* s="(((A, B), C, (D, E, F), G, (J, K, L), M, (N, O)), "
      "P, ((Q, R, S), T, (U, V), X, (Y, Z)))";
    typedef BTree<std::string, 3> BTr;
    BTr* tr = str_to_btree<BTr>(s);
    std::cout<<"string fed:\n"<<s<<"\n"
             <<"parsed res:\n"<<btree_to_str(tr)<<"\n";
    delete tr;
  }
};

#endif //_B_TREE_

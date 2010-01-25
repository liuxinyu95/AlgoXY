//     inttrie.hpp, Integer based Trie
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

#ifndef _INT_TRIE
#define _INT_TRIE
#include <iostream>
#include <string>
#include <sstream>
#include <list>
#include <iterator>

template<class T>
struct IntTrie{
  IntTrie():value(), left(0), right(0){}
  ~IntTrie(){
    delete left;
    delete right;
  }
  T value;
  IntTrie* left;
  IntTrie* right;
};

template<class T>
IntTrie<T>* insert(IntTrie<T>* t, int key, T value=T()){
  if(!t)
    t = new IntTrie<T>();

  IntTrie<T>* p = t;
  while(key){
    if( (key&0x1) == 0){
      if(!p->left) p->left = new IntTrie<T>();
      p = p->left;
    }
    else{
      if(!p->right) p->right = new IntTrie<T>();
      p = p->right;
    }
    key>>=1;
  }
  p->value = value;
  return t;
}

template<class T>
T lookup(IntTrie<T>* t, int key){
  while(key && t){
    if( (key & 0x1) == 0)
      t = t->left;
    else
      t = t->right;
    key>>=1;
  }
  if(t)
    return t->value;
  else
    return T();
}

template<class T>
std::string trie_to_str(IntTrie<T>* t, int prefix=0, int depth=0){
  std::stringstream s;
  s<<"("<<prefix;
  if(t->value!=T())
    s<<":"<<t->value;
  if(t->left)
    s<<", "<<trie_to_str(t->left, prefix, depth+1);
  if(t->right)
    s<<", "<<trie_to_str(t->right, (1<<depth)+prefix, depth+1);
  s<<")";
  return s.str();
}

template<class T, class Iterator>
IntTrie<T>* list_to_trie(Iterator first, Iterator last){
  IntTrie<T>* t(0);
  for(;first!=last; ++first)
    t = insert(t, *first);
  return t;
}

template<class T, class Iterator>
IntTrie<T>* map_to_trie(Iterator first, Iterator last){
  IntTrie<T>* t(0);
  for(;first!=last; ++first)
    t = insert(t, first->first, first->second);
  return t;
}

class IntTrieTest{
public:
  IntTrieTest():ti(0), tc(0){}

  ~IntTrieTest(){
    delete ti;
    delete tc;
  }

  void run(){
    test_trie_insert();
    test_lookup();
  }
private:
  void test_trie_insert(){
    const int lst[] = {1, 4, 5};
    std::list<int> l(lst, lst+sizeof(lst)/sizeof(int));

    ti = list_to_trie<int, std::list<int>::iterator>(l.begin(), l.end());
    std::copy(l.begin(), l.end(), 
              std::ostream_iterator<int>(std::cout, ", "));
    std::cout<<"==>"<<trie_to_str(ti)<<"\n";
    
    typedef std::list<std::pair<int, char> > Dict;
    const int  keys[] = {4, 1, 5, 9};
    const char vals[] = "bacd";
    Dict m;
    for(unsigned int i=0; i<sizeof(keys)/sizeof(int); ++i)
      m.push_back(std::make_pair(keys[i], vals[i]));
    tc = map_to_trie<char, Dict::iterator>(m.begin(), m.end());
    std::copy(keys, keys+sizeof(keys)/sizeof(int),
              std::ostream_iterator<int>(std::cout, ", "));
    std::cout<<"==>"<<trie_to_str(tc);
  }

  void test_lookup(){
    std::cout<<"\nlook up 4: "<<lookup(tc, 4)
             <<"\nlook up 9: "<<lookup(tc, 9)
             <<"\nlook up 0: "<<lookup(tc, 0)<<"\n";
  }

  IntTrie<int>* ti;
  IntTrie<char>* tc;
};

#endif //_INT_TRIE

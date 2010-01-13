#ifndef _TRIE_UTILITY
#define _TRIE_UTILITY

#include <sstream>
#include <functional> //for std::ptr_fun
#include <numeric> //for std::accumulate

template<class T>
std::string trie_to_str(T* t, std::string prefix=""){
  std::ostringstream s;
  s<<"("<<prefix;
  if(t->value != typename T::ValueType())
    s<<":"<<t->value;
  for(typename T::Children::iterator it=t->children.begin();
      it!=t->children.end(); ++it)
    s<<", "<<trie_to_str(it->second, prefix+it->first);
  s<<")";
  return s.str();
}

// for C++ fold-left, std::accumulate
template<class T, class Key>
T* insert_key(T* t, Key key){
  return insert(t, key);
}

// list_to_trie
template<class Iterator, class T>
T* list_to_trie(Iterator first, Iterator last, T* t){
  return std::accumulate(first, last, t,
                         std::ptr_fun(insert_key<T, typename T::KeyType>));
}

#endif //_TRIE_UTILITY

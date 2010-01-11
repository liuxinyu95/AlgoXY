#ifndef _TRIE_UTILITY
#define _TRIE_UTILITY

#include <sstream>

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
template<class T, class K>
T* insert_key(T* t, K key){
  return insert(t, key);
}

#endif //_TRIE_UTILITY

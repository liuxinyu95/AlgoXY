#ifndef _TRIE_
#define _TRIE_

#include <map>
#include <string>
#include <iostream>
#include <iterator>
#include "trieutil.hpp"

// Definition
template<class Key, class Value>
struct Trie{
  typedef typename Key::value_type Char;
  typedef Trie<Key, Value> Self;
  typedef std::map<Char, Self*> Children;
  typedef Key KeyType;
  typedef Value ValueType;

  Trie():value(Value()){}

  virtual ~Trie(){
    for(typename Children::iterator it=children.begin();
        it!=children.end(); ++it)
      delete it->second;
  }

  Value value;
  Children children;
};

template<class K, class V>
Trie<K, V>* insert(Trie<K, V>* t, 
                   typename Trie<K, V>::KeyType key, 
                   typename Trie<K, V>::ValueType value=V()){
  if(!t)
    t = new Trie<K, V>();

  Trie<K, V>* p(t);
  for(typename K::iterator it=key.begin(); it!=key.end(); ++it){
    if(p->children.find(*it) == p->children.end())
      p->children[*it] = new Trie<K, V>();
    p = p->children[*it];
  }
  p->value = value;
  return t;
}

template<class K, class V>
V lookup(Trie<K, V>* t, typename Trie<K, V>::KeyType key){
  if(!t)
    return V(); //or throw exception

  Trie<K, V>* p(t);
  for(typename K::iterator it=key.begin(); it!=key.end(); ++it){
    if(p->children.find(*it) == p->children.end())
      return V(); //or throw exception
    p = p->children[*it];
  }
  return p->value;
}

class TrieTest{
public:
  void run(){
    std::cout<<"\ntest alphabetic trie\n";
    test_insert();
    test_lookup();
  }
private:
  void test_insert(){
    typedef Trie<std::string, std::string> TrieType;
    TrieType* t(0);
    const char* lst[] = {"a", "an", "another", "b", "bob", "bool", "home"};
    t = list_to_trie(lst, lst+sizeof(lst)/sizeof(char*), t);
    std::copy(lst, lst+sizeof(lst)/sizeof(char*),
	      std::ostream_iterator<std::string>(std::cout, ", "));
    std::cout<<"\n==>"<<trie_to_str(t)<<"\n";
    delete t;

    t=0;
    const char* keys[] = {"001", "100", "101"};
    const char* vals[] = {"y", "x", "z"};
    for(unsigned int i=0; i<sizeof(keys)/sizeof(char*); ++i)
      t = insert(t, std::string(keys[i]), std::string(vals[i]));
    std::copy(keys, keys+sizeof(keys)/sizeof(char*),
	      std::ostream_iterator<std::string>(std::cout, ", "));
    std::cout<<"==>"<<trie_to_str(t)<<"\n";
    delete t;
  }

  void test_lookup(){
    Trie<std::string, int>* t(0);
    const char* keys[] = {"a", "an", "another", "b", "bool", "bob", "home"};
    const int vals[] = {1, 2, 7, 1, 4, 3, 4};
    for(unsigned int i=0; i<sizeof(keys)/sizeof(char*); ++i)
      t = insert(t, std::string(keys[i]), vals[i]);
    std::cout<<"\nlookup another: "<<lookup(t, "another")
	     <<"\nlookup home: "<<lookup(t, "home")
	     <<"\nlookup the: "<<lookup(t, "the")<<"\n";
    delete t;
  }
};

#endif //_TRIE_


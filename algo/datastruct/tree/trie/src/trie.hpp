#ifndef _TRIE_
#define _TRIE_

#include <map>
#include <string>
#include <sstream>
#include <iostream>

// Definition
template<class KeyType = std::string, class ValueType = KeyType>
struct Trie{
  typedef Trie<KeyType, ValueType> Self;
  typedef std::map<KeyType, Self> Children;

  Trie():value(ValueType()){}

  virtual ~Trie(){
    for(typename Children::iterator it=children.begin();
        it!=children.end(); ++it)
      delete it->second;
  }

  ValueType value;
  Children children;
};

template<class Key, class Value>
Trie<Key, Value>* insert(Trie<Key, Value>* t, Key key, Value value=Value()){
  if(!t)
    t = Trie<Key, Value>();

  Trie<Key, Value>* p(t);
  for(typename Key::iterator it = key.begin(); it!=key.end(); ++it){
    if(p->children.find(*it) == p->children.end())
      p->children[*it] = new Trie<Key, Value>();
    p = p->children[*it];
  }
  return t;
}

// helper functions

template<class K, class V>
std::string trie_to_str(Trie<K, V>* t, std::string prefix=""){
  std::ostringstream s;
  s<<"("<<prefix;
  if(t->value != V())
    s<<":"<<t->value;
  for(typename Trie<K, V>::Children::iterator it=t->children.begin();
      it!=t->children.end(); ++it)
    s<<", "<<trie_to_str(it->second, prefix+it->first);
  s<<")";
  return s.str();
}

class TrieTest{
public:
  TrieTest():t(0){}

  ~TrieTest(){
    delete t;
  }

  void run(){
    test_insert();
    test_lookup();
  }
private:
  void test_insert(){
  }

  void test_lookup(){
  }

  Trie t;
};

//endif //_TRIE_


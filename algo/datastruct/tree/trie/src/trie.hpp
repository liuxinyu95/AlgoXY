#ifndef _TRIE_
#define _TRIE_

#include <map>
#include <string>
#include <sstream>
#include <iostream>
#include <iterator>
#include <functional> //for std::ptr_fun
#include <numeric> //for std::accumulate

// Definition
template<class Char, class Value>
struct Trie{
  typedef Trie<Char, Value> Self;
  typedef std::map<Char, Self*> Children;
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

template<class Char, class Value, class Key>
Trie<Char, Value>* insert(Trie<Char, Value>* t, Key key, Value value=Value()){
  if(!t)
    t = new Trie<Char, Value>();

  Trie<Char, Value>* p(t);
  for(typename Key::iterator it = key.begin(); it!=key.end(); ++it){
    if(p->children.find(*it) == p->children.end())
      p->children[*it] = new Trie<Char, Value>();
    p = p->children[*it];
  }
  return t;
}

// helper functions

// for C++ fold-left, std::accumulate
template<class T, class K>
T* insert_key(T* t, K key){
  return insert(t, key);
}

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

class TrieTest{
public:
  TrieTest():t(0){}

  ~TrieTest(){
    delete t;
  }

  void run(){
    std::cout<<"\ntest alphabetic trie\n";
    test_insert();
    test_lookup();
  }
private:
  void test_insert(){
    const char* lst[] = {"a", "an", "another", "b", "bob", "bool", "home"};
    t = std::accumulate(lst, lst+sizeof(lst)/sizeof(char*), t,
			std::ptr_fun(insert_key<TrieType, std::string>));
    std::copy(lst, lst+sizeof(lst)/sizeof(char*),
	      std::ostream_iterator<std::string>(std::cout, ", "));
    std::cout<<"\n==>"<<trie_to_str(t)<<"\n";
  }

  void test_lookup(){
  }

  typedef Trie<char, std::string> TrieType;
  Trie<char, std::string>* t;
};

#endif //_TRIE_


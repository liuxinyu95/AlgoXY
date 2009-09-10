#include <iostream>
#include <map>
#include <functional>
#include <sstream>
#include <algorithm>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/construct.hpp>

template<class T>
struct Trie{
  typedef std::map<T, Trie<T>*> Children;

  Trie(int x=-1):value(x){}

  ~Trie(){
    for(typename Children::iterator it=children.begin();
        it!=children.end(); ++it)
      delete it->second;
  }

  int value; //frequency etc.
  Children children;
};

// recursive
template<class T, class Coll>
Trie<T>* insert(Trie<T>* t, Coll value){
  if(!t)
    t=new Trie<T>;

  if(!value.empty()){
    typename Coll::iterator it=value.begin();
    t->children[*it]=insert(t->children[*it], 
                            Coll(++value.begin(), value.end())); //tricky, we can't use Coll(++it, value.end()), because ++it is evalueate first before *it.
  }
  return t;
}

// test helpers
template<class T>
std::string trie_to_str(Trie<T>* t, std::string prefix=""){
  std::ostringstream s;
  s<<"("<<prefix;
  for(typename Trie<T>::Children::iterator it=t->children.begin();
      it!=t->children.end(); ++it)
    s<<", "<<trie_to_str(it->second, prefix+it->first);
  s<<")";

  return s.str();
}

void test_insert(){
  Trie<char>* t(0);
  t=insert(t, std::string("a"));
  t=insert(t, std::string("b"));
  insert(t, std::string("good"));
  insert(t, std::string("home"));
  insert(t, std::string("gone"));
  std::cout<<trie_to_str(t);
  delete t;
}

int main(int, char**){
  test_insert();
}

#ifndef _EDICT_
#define _EDICT_

#include <list>
#include <queue>
#include <algorithm> //for std::equal
#include "trie.hpp"
#include "patricia.hpp"

template<class T>
std::list<std::pair<typename T::KeyType, typename T::ValueType> >
expand(typename T::KeyType prefix, T* t, unsigned int n)
{
  typedef typename T::KeyType KeyType;
  typedef typename T::ValueType ValueType;
  typedef std::list<std::pair<KeyType, ValueType> > Result;

  Result res;
  std::queue<std::pair<KeyType, T*> > q;
  q.push(std::make_pair(prefix, t));
  while(res.size()<n && (!q.empty())){
    std::pair<KeyType, T*> i = q.front();
    KeyType s = i.first;
    T* p = i.second;
    q.pop();
    if(p->value != ValueType()){
      res.push_back(std::make_pair(s, p->value));
    }
    for(typename T::Children::iterator it = p->children.begin();
        it!=p->children.end(); ++it)
      q.push(std::make_pair(s+it->first, it->second));
  }
  return res;
}

//lookup top n candidate with prefix key in Trie
template<class K, class V>
std::list<std::pair<K, V> > lookup(Trie<K, V>* t, 
                                   typename Trie<K, V>::KeyType key, 
                                   unsigned int n)
{
  typedef std::list<std::pair<K, V> > Result;
  if(!t)
    return Result();

  Trie<K, V>* p(t);
  for(typename K::iterator it=key.begin(); it!=key.end(); ++it){
    if(p->children.find(*it) == p->children.end())
      return Result();
    p = p->children[*it];
  }
  return expand(key, p, n);
}

// x `is prefix of` y?
template<class T>
bool is_prefix_of(T x, T y){
  if(x.size()<=y.size())
    return std::equal(x.begin(), x.end(), y.begin());
  return false;
}

template<class K, class V>
std::list<std::pair<K, V> > lookup(Patricia<K, V>* t, 
                                   typename Patricia<K, V>::KeyType key,
                                   unsigned int n)
{
  typedef typename std::list<std::pair<K, V> > Result;
  typedef typename Patricia<K, V>::Children::iterator Iterator;
  if(!t)
    return Result();
  K prefix;
  for(;;){
    bool match(false);
    for(Iterator it=t->children.begin(); it!=t->children.end(); ++it){
      K k(it->first);
      if(is_prefix_of(key, k))
        return expand(prefix+k, it->second, n);
      if(is_prefix_of(k, key)){
        match = true;
        prefix += k;
        lcp<K>(key, k); //update key
        t = it->second;
        break;
      }
    }
    if(!match)
      return Result();
  }
}

//list of pairs to string
template<class Container>
std::string lop_to_str(Container coll){
  typedef typename Container::iterator Iterator;
  std::ostringstream s;
  s<<"[";
  for(Iterator it=coll.begin(); it!=coll.end(); ++it)
    s<<"("<<it->first<<", "<<it->second<<"), ";
  s<<"]";
  return s.str();
}

class EDictTest{
public:
  EDictTest():t(0), p(0){
    const char* dict[] = {
      "a", "the first letter of English",                               \
      "an", "used instead of 'a' when the following word begins with a vowel sound", \
      "another", "one more person or thing or an extra amount",          \
      "abandon", "to leave a place, thing or person forever",            \
      "about", "on the subject of; connected with",                      \
      "adam", "a character in the Bible who was the first man made by God", \
      "boy", "a male child or, more generally, a male of any age",       \
      "body", "the whole physical structure that forms a person or animal", \
      "zoo", "an area in which animals, especially wild animals, are kept so that people can go and look at them, or study them"};

    const char** first=dict;
    const char** last =dict + sizeof(dict)/sizeof(char*);
    for(;first!=last; ++first, ++first){
      t = insert(t, *first, *(first+1));
      p = insert(p, *first, *(first+1));
    }
  }

  ~EDictTest(){
    delete t;
    delete p;
  }

  void run(){
    std::cout<<"\nword auto-completion and T9 test\n";
    test_trie_lookup();
    test_patricia_lookup();
  }

private:
  void test_trie_lookup(){
    std::cout<<"test lookup top 5 in Trie\n"
             <<"search a "<<lop_to_str(lookup(t, "a", 5))<<"\n"
             <<"search ab "<<lop_to_str(lookup(t, "ab", 5))<<"\n";
  }

  void test_patricia_lookup(){
    std::cout<<"\ntest lookup top 5 in Patricia\n"
             <<"search a "<<lop_to_str(lookup(p, "a", 5))<<"\n"
             <<"search ab "<<lop_to_str(lookup(p, "ab", 5))<<"\n";
  }

  Trie<std::string, std::string>* t;
  Patricia<std::string, std::string>* p;
};

#endif //_EDICT_

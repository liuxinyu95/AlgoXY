#include <iostream>
#include <map>

//
// Typically, trie use map to store children.
// I use multimap to support T9 like 1 key - multiple values cases
//
template<class T, typename Compare=std::less<T> >
struct Trie{
  Trie(Compare comp=std::less<T>):children(comp){}
  Trie(T x, Compare comp=std::less<T>):children(comp){ 
    children[x]=new Trie; 
  }
  ~Trie(){
    for(Children::iterator it=children.begin(); 
        it!=children.end(); ++it)
      delete it->second;
  }
  typedef std::multimap<T, Trie<T>*, Compare> Children;
  Children children;
};

//
// Functional approach can be used if we assume there
// is no too long word, that will cause stack overflow.
//
template<class T, class Coll, 
         typename Compare, typename MapFunc=std::identity<T> >
Trie<T, Compare>* insert(Trie<T, Compare>* t, Coll value, MapFunc f=MapFunc()){
  if(!t)
    t=new Trie<T, Compare>;

  if(value.empty())
    return t;
  
  typename Coll::iterator it=value.begin();
  return insert(t->children[f(*it)], Coll(++it, value.end()));
}

//
// For T9 input method,
// returns candidates list, the input string is ITU-T keys(2..9)
// the compare function is customized to map from key to char.
//
// For input completion,
// returns a common parent of candidates words.
//
template<class T, typename Coll, typename Comp=std::equal_to<T> >
std::list<Trie<T>*> search(Trie<T>* t, Coll str, Comp comp=Comp()){
  typedef std::list<Trie<T>*> Result;
  Result res;
  res.push_back(t);
  for(Coll::iterator x=str.begin(); x!=str.end(); ++x){
    Result candidates;
    for(Result::iterator it=res.begin(); it!=res.end(); ++it){
      if(it->children.find
    }
  }
  return res;
}

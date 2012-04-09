//     stree.hpp, Suffix Tree online construction algorithm in Ukkonen '95
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

#ifndef _SUFFIX_TREE_
#define _SUFFIX_TREE_

#include <iostream>
#include <map>
#include <string>
#include <iterator>
#include <sstream>
#include <list>
#include <algorithm>
//#include "boost/tuple/tuple.hpp"
#include "streeutil.hpp"

//
// Simulate boost::tie
//
template<typename T1, typename T2>
struct Bind{
  Bind(T1& r1, T2& r2):x1(r1), x2(r2){}
  Bind(const Bind& r):x1(r.x1), x2(r.x2){}

  // Support implicit type conversion
  template<typename U1, typename U2>
  Bind& operator=(const std::pair<U1, U2>& p){
    x1 = p.first;
    x2 = p.second;
    return *this;
  }
  T1& x1;
  T2& x2;
};

template<typename T1, typename T2>
Bind<T1, T2> tie(T1& r1, T2& r2){ 
  return Bind<T1, T2>(r1, r2); 
}

//
// In order to improve the efficiency by operation on-line
// There is only one copy of the string. All sub strings are
// represented as reference pair: 
//   w = (left, right)
//
struct StrRef: public std::pair<int, int>{
  typedef std::pair<int, int> Pair;
  static std::string str;
  StrRef():Pair(){}
  StrRef(int l, int r):Pair(l, r){}
  StrRef(const Pair& ref):Pair(ref){}

  std::string substr(){
    int l, r;
    tie(l, r) = *this;
    return str.substr(l, len());
  }

  int len(){ 
    int l, r;
    tie(l, r) = *this;
    return r-l+1; 
  }
};

std::string StrRef::str="";

struct Node;

struct RefPair: public std::pair<Node*, StrRef>{
  typedef std::pair<Node*, StrRef> Pair;
  RefPair():Pair(){}
  RefPair(Node* n, StrRef s):Pair(n, s){}
  RefPair(const Pair& p):Pair(p){}
  Node* node(){ return first; }
  StrRef str(){ return second; }
};

struct Node{
  typedef std::string::value_type Key;
  typedef std::map<Key, RefPair> Children;

  Node():suffix(0){}
  ~Node(){
    for(Children::iterator it=children.begin();
        it!=children.end(); ++it)
      delete it->second.node();
  }

  Children children;
  Node* suffix;
};

struct STree{
  STree(std::string s):str(s), 
                       infinity(s.length()+1000), 
                       root(new Node)
  { StrRef::str = str; }

  ~STree() { delete root; }
  std::string str;
  int infinity;
  Node* root;
};

//
// branch: 
//  this is test-and-split function in Ukkonen '95
//  test if (node, str_ref) is an EP, (EP has a c-transition)
//  if not, then branch out a new node
//
Node* branch(STree* t, Node* node, StrRef str, Node::Key c){
  int l, r;
  tie(l, r) = str;
  if(str.len()<=0){ // (node, empty)
    if(node && node->children.find(c)==node->children.end())
      return node;
    else
      return 0; // either node is empty (_|_), or is EP
  }
  else{
    RefPair rp = node->children[t->str[l]];
    int l1, r1;
    tie(l1, r1) = rp.str();
    int pos = l1+str.len();
    if(t->str[pos]==c)
      return 0;
    else{ // node--->branch_node--->node1
      Node* branch_node = new Node();
      node->children[t->str[l1]]=RefPair(branch_node, StrRef(l1, pos-1));
      branch_node->children[t->str[pos]] = RefPair(rp.node(), StrRef(pos, r1));
      return branch_node;
    }
  }
}

//
// node[c]--->(l, r), _ 
// node[c]--->((l', r'), node')--->...-->((l'', r''), node'')--->((x, inf), leaf)
// where _: it may not be a node, but some implicity position
// find the closet node and left, so that they point to same position _
// special case: ||None, (k, p)|| = (root, (k+1, p))
//
std::pair<Node*, int> canonize(STree* t, Node* node, StrRef str){
  int l, r;
  tie(l, r)=str;
  if(!node){
    if(str.len()<=0)
      return std::make_pair(node, l);
    else
      return canonize(t, t->root, StrRef(l+1, r));
  }
  while(l<=r){ //str isn't empty
    RefPair rp = node->children[t->str[l]];
    int l1, r1;
    tie(l1, r1) = rp.str();
    if(r-l >= r1-l1){
      l += rp.str().len(); // remove len() from (l, r)
      node = rp.node();
    }
    else
      break;
  }
  return std::make_pair(node, l);
}

//
// Main func: STree(Str[i-1]) ==> STree(Str[i])
//   param: (node, (l, i-1)): AP (Active Point)
//   prev: oldr in Ukkonen '95
//   p:    r in Ukkonen '95
//   return, EP (End Point) ref pair (node, (l, i-1))
//           since r always = i-1, just return (node, l)
//
std::pair<Node*, int> update(STree* t, Node* node, StrRef str){
  int l, i;
  tie(l, i)=str;
  Node::Key c(t->str[i]); //current char
  Node dummy, *p;
  Node* prev(&dummy);
  while((p=branch(t, node, StrRef(l, i-1), c))!=0){
    p->children[c]=RefPair(new Node(), StrRef(i, t->infinity));
    prev->suffix = p;
    prev = p;
    // go up along suffix link
    tie(node, l) = canonize(t, node->suffix, StrRef(l, i-1));
  }
  prev->suffix = node;
  return std::make_pair(node, l);
}

//
// Algorithm 2 in Ukkonen '95
//   l: left index of a word, k in Ukkonen '95
//   node: s in Ukkonen '95
//   None: _|_ in Ukkonen '95
//
STree* suffix_tree(std::string s){
  STree* t=new STree(s);
  Node* node = t->root; // init active point as (root, empty)
  for(unsigned int i=0, l=0; i<s.length(); ++i){
    tie(node, l) = update(t, node, StrRef(l, i));
    tie(node, l) = canonize(t, node, StrRef(l, i));
  }
  return t;
}

//
// helper functions for output
//

std::list<std::string> to_lines(Node* node){
  typedef std::list<std::string> Result;
  Result res;
  if(node->children.empty()){
    res.push_back("");
    return res;
  }
  for(Node::Children::iterator it = node->children.begin();
      it!=node->children.end(); ++it){
    RefPair rp = it->second;
    Result lns = to_lines(rp.node());
    std::string edge = rp.str().substr();
    *lns.begin() = "|--" + edge + "-->" + (*lns.begin());
    map_add(++lns.begin(), lns.end(), 
            std::string("|")+std::string(edge.length()+5, ' '));
    if(!res.empty())
      res.push_back("|");
    concat(res, lns);
  }
  return res;
}

std::string to_str(STree* t){
  std::list<std::string> ls = to_lines(t->root);
  std::ostringstream s;
  std::copy(ls.begin(), ls.end(), 
            std::ostream_iterator<std::string>(s, "\n"));
  return s.str();
}

class SuffixTreeTest{
public:
  SuffixTreeTest(){
    std::cout<<"Start suffix tree test\n";
  }
  void run(){
    test_build("cacao");
    test_build("mississippi");
    test_build("banana$");
  }
private:
  void test_build(std::string str){
    for(unsigned int i=0; i<str.length(); ++i)
      test_build_step(str.substr(0, i+1));
  }

  void test_build_step(std::string str){
    STree* t = suffix_tree(str);
    std::cout<<"Suffix Tree ("<<str<<"):\n"
             <<to_str(t)<<"\n";
    delete t;
  }
};

#endif //_SUFFIX_TREE_

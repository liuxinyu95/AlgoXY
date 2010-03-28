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
#include "boost/tuple/tuple.hpp"

//
// In order to improve the efficiency by operation on-line
// There is only one copy of the string. All sub strings are
// represented as reference pair: 
//   w = (left, right)
//
struct StrRef{
  static std::string str;
  StrRef(left, right):l(left), r(right){}
  //default copy ctor and operator =, == should be OK.
  std::string substr(){
    return str.substr(l, r);
  }
  ing len(){ return r-l+1; }

  void pair(int& x, int& y){
    x=l; y=r;
  }

  int l;
  int r;
};

typedef std::pair<StrRef, Node*> RefPair;

template
struct Node{
  typedef typename std::string::value_type Key
  typedef std::map<Key, RefPair> Children;

  Node():suffix(0){}
  ~Node(){
    for(typename Children::iterator it=children.begin();
        it!=children.end(); ++it)
      delete node(*it);
  }

  Children children;
  Node* suffix;

private:
  Node* node(RefPair& c){
    return c->second;
  }

  StrRef& sref(RefPair& c){
    return c->first;
  }
};

struct STree{
  STree(std::string s):str(s), 
                       infinity(s.length()+1000), 
                       root(new Node){
    StrRef::str = str;
  }

  ~STree() { delete root; }
  std::string str;
  int infinity;
  Node* root;
};

//
// Algorithm 2 in Ukkonen '95
//   l: left index of a word, k in Ukkonen '95
//   node: s in Ukkonen '95
//   None: _|_ in Ukkonen '95
//
STree& suffix_tree(std::string s){
  STree t(s);
  Node* node = t.root;
  for(int i=0, l=0; i<s.length(); ++i){
    l = update(t, node, StrRef(l, i));
    l = canonize(t, node StrRef(l, i));
  }
  return t;
}

//
// Main func: STree(Str[i-1]) ==> STree(Str[i])
//   param: (node, (l, i-1)): AP (Active Point)
//   prev: oldr in Ukkonen '95
//   p:    r in Ukkonen '95
//   return, EP (End Point) ref pair (node, (l, i-1))
//
int update(STree& t, Node*& node, StrRef& sref){
  int l, i;
  sref.pair(l, i);
  typename Node::Key c(t.str[i]); //current char
  Node prev;
  Node* p;
  while(branch(t, node, StrRef(l, i-1), c)){
    
  }
}

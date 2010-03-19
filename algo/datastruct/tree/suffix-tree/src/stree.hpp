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

struct StrRef{
  StrRef(left, right):l(left), r(right){}
  //default copy ctor and operator =, == should be OK.
  std::string substr(){
    return str.substr(l, r);
  }
  ing len(){ return r-l+1; }
  static std::string str;
  int l;
  int r;
};

template<Key>
struct Node{
  typedef std::pair<StrRef, Node*> RefPair;
  typedef std::map<Key, RefPair> Children;

  Node():suffix(0){}
  ~Node(){
    for(typename Children::iterator it=children.begin();
        it!=children.end(); ++it)
      delete it->second->second
  }

  Children children;
  Node* suffix;
private:
  Node* node(RefPair& c){
  }
};

struct STree{
  std::string str;
  int infinity;
  
};

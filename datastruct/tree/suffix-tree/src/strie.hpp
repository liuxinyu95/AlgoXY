//     strie.hpp, Suffix Trie online construction algorithm.
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

#ifndef _SUFFIX_TRIE_
#define _SUFFIX_TRIE_

#include <iostream>
#include <map>
#include <string>
#include <iterator>
#include <sstream>
#include <list>
#include <algorithm>
#include <functional> //std::ptr_fun
#include <numeric> //std::accumulate
#include "streeutil.hpp"

namespace STrie{

  struct Node{
    typedef std::string::value_type Key;
    typedef std::map<Key, Node*> Children;

    Node(Node* suffix_link=0):suffix(suffix_link){}
    ~Node(){
      for(Children::iterator it=children.begin();
          it!=children.end(); ++it)
        delete it->second;
    }

    Children children;
    Node* suffix;
  };

  Node* insert(Node* top, Node::Key c){
    if(!top)
      top = new Node();
    Node dummy;
    Node *node(top), *prev(&dummy);
    while(node && (node->children.find(c)==node->children.end())){
      prev->suffix = node->children[c] = new Node(node);
      prev = prev->suffix;
      node = node->suffix;
    }
    if(node)
      prev->suffix = node->children[c];
    return top->children[c];
  }

  Node* root(Node* node){
    for(; node->suffix; node=node->suffix);
    return node;
  }

  Node* suffix_trie(std::string s){
    return root(std::accumulate(s.begin(), s.end(), (Node*)0, 
                                std::ptr_fun(insert)));
  }

  std::list<std::string> to_lines(Node* node){
    typedef std::list<std::string> Result;
    Result res;
    if(node->children.empty()){
      res.push_back("");
      return res;
    }
    for(Node::Children::iterator it = node->children.begin();
        it!=node->children.end(); ++it){
      Result lns = to_lines(it->second);
      *lns.begin() = std::string("|--") + it->first + "-->" + (*lns.begin());
      map_add(++lns.begin(), lns.end(), std::string("|      "));
      if(!res.empty())
        res.push_back("|");
      concat(res, lns);
    }
    return res;
  }

  std::string to_str(Node* t){
    std::list<std::string> ls = to_lines(t);
    std::ostringstream s;
    std::copy(ls.begin(), ls.end(), 
              std::ostream_iterator<std::string>(s, "\n"));
    return s.str();
  }
} //namespace STrie

class SuffixTrieTest{
public:
  SuffixTrieTest(){
    std::cout<<"Start suffix Trie test\n";
  }
  void run(){
    test_build("cacao");
    test_build("banana$");
    test_build("mississippi");
  }
private:
  void test_build(std::string s){
    for(unsigned int i=0; i<s.length(); ++i)
      test_build_step(s.substr(0, i+1));
  }

  void test_build_step(std::string s){
    STrie::Node* t = STrie::suffix_trie(s);
    std::cout<<"Suffix Trie ("<<s<<"):\n"
             <<STrie::to_str(t)<<"\n";
    delete t;
  }
};

#endif //_SUFFIX_TRIE_

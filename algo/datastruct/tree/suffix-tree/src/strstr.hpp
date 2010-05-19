//     strstr.hpp, String manipulation over suffix tree
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

#ifndef _STREE_STR_
#define _STREE_STR_

#include "stree.hpp"
#include <queue>

// Special terminator
const char TERM1 = '$';
const char TERM2 = '#';

typedef std::list<std::string> Strings;

void update_max(Strings& res, std::string s){
  if(res.empty() || (*res.begin()).length() < s.length()){
    res.clear();
    res.push_back(s);
    return;
  }
  if((*res.begin()).length() == s.length())
    res.push_back(s);
}

// Search the longest repeated substringS
Strings lrs(const STree* t){
  std::queue<std::pair<std::string, Node*> > q;
  Strings res;
  q.push(std::make_pair(std::string(""), t->root));
  while(!q.empty()){
    std::string s;
    Node* node;
    tie(s, node) = q.front();
    q.pop();
    for(Node::Children::iterator it = node->children.begin();
        it!=node->children.end(); ++it){
      RefPair rp = it->second;
      if(!(rp.node()->children.empty())){
        std::string s1 = s + rp.str().substr();
        q.push(std::make_pair(s1, rp.node()));
        update_max(res, s1);
      }
    }
  }
  return res;
} 

class STreeUtil{
public:
  STreeUtil():t(0){}
  ~STreeUtil(){ delete t; }

  Strings find_lrs(std::string s){
    lazy(s);
    return lrs(t);
  }

private:
  void lazy(std::string s){
    if((!t) || t->str != s+TERM1){
      delete t;
      t = suffix_tree(s+TERM1);
    }
  }
  STree* t;
};

std::ostream& operator<<(std::ostream& s, Strings lst){
  std::ostringstream ss;
  std::copy(lst.begin(), lst.end(),
            std::ostream_iterator<std::string>(ss, ", "));
  return s<<"["<<ss.str()<<"]";
}

class StrSTreeTest{
public:
  StrSTreeTest(){
    std::cout<<"start string manipulation over suffix tree test\n";
  }

  void run(){
    test_lrs();
  }

  void test_lrs(){
    __test_lrs("mississippi");
    __test_lrs("banana");
    __test_lrs("cacao");
    __test_lrs("foofooxbarbar");
  }
private:
  void __test_lrs(std::string s){
    std::cout<<"longest repeated substirng of "<<s<<"="
             <<util.find_lrs(s)<<"\n";
  }
  STreeUtil util;
};
#endif //_STREE_STR_

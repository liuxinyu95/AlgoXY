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

bool is_leaf(Node* node){
  return node->children.empty();
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
      if(!is_leaf(rp.node())){
        std::string s1 = s + rp.str().substr();
        q.push(std::make_pair(s1, rp.node()));
        update_max(res, s1);
      }
    }
  }
  return res;
} 

bool match_fork(Node* node){
  bool res(false);
  if(node->children.size() == 2)
    for(Node::Children::iterator it = node->children.begin();
        it!=node->children.end(); ++it){
      RefPair rp = it->second;
      if(!is_leaf(rp.node()))
         return false;
      if(rp.str().substr().find(TERM2)!=std::string::npos)
        res = true;
    }
  return res;
}

Strings lcs(const STree* t){
  std::queue<std::pair<std::string, Node*> > q;
  Strings res;
  q.push(std::make_pair(std::string(""), t->root));
  while(!q.empty()){
    std::string s;
    Node* node;
    tie(s, node) = q.front();
    q.pop();
    if(match_fork(node))
      update_max(res, s);
    for(Node::Children::iterator it = node->children.begin();
        it!=node->children.end(); ++it){
      RefPair rp = it->second;
      if(!is_leaf(rp.node())){
        std::string s1 = s + rp.str().substr();
        q.push(std::make_pair(s1, rp.node()));
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

  Strings find_lcs(std::string s1, std::string s2){
    lazy(s1+TERM2+s2);
    return lcs(t);
  }

  Strings find_lpalindrome(std::string s){
    std::string s1(s);
    std::reverse(s1.begin(), s1.end());
    return find_lcs(s, s1);
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
    test_lcs();
    test_lpalindrome();
  }

  void test_lrs(){
    __test_lrs("mississippi");
    __test_lrs("banana");
    __test_lrs("cacao");
    __test_lrs("foofooxbarbar");
  }

  void test_lcs(){
    __test_lcs("ababx", "baby");
  }

  void test_lpalindrome(){
    __test_lpalindrome("mississippi");
    __test_lpalindrome("banana");
    __test_lpalindrome("cacao");
    __test_lpalindrome("Woolloomooloo");
  }

private:
  void __test_lrs(std::string s){
    std::cout<<"longest repeated substirng of "<<s<<"="
             <<util.find_lrs(s)<<"\n";
  }

  void __test_lcs(std::string s1, std::string s2){
    std::cout<<"longest common substring of "<<s1<<", "<<s2<<" ="
             <<util.find_lcs(s1, s2)<<"\n";
  }

  void __test_lpalindrome(std::string s){
    std::cout<<"longest palindrome of "<<s<<" ="
             <<util.find_lpalindrome(s)<<"\n";
  }

  STreeUtil util;
};
#endif //_STREE_STR_

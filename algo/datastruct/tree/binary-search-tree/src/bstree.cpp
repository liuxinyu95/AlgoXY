/*
 * bstree.cpp
 * Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
#include <iostream>
#include <vector>
#include <sstream>
//#include <boost/lambda/lambda.hpp>

template<class T>
struct node{
  node(T x):key(x), left(0), right(0), parent(0){}
  ~node(){ // for convinient, use functional approach
    delete left;
    delete right;
  }

  node* left; 
  node* right;
  node* parent; //parent is optional, it's helpful for succ/pred
  T key;
};

// in-order tree walk
// easy implemented by using functional approach
template<class T, class F>
void in_order_walk(node<T>* t, F f){
  if(t){
    in_order_walk(t->left, f);
    f(t->key);
    in_order_walk(t->right, f);
  }
}

template<class T>
node<T>* search(node<T>* t, T x){
  while(t && t->key!=x){
    if(x < t->key) t=t->left;
    else t=t->right;
  }
  return t;
}

template<class T>
node<T>* min(node<T>* x){
  while(x && x->left)
    x = x->left;
  return x;
}

template<class T>
node<T>* max(node<T>* x){
  while(x && x->right)
    x = x->right;
  return x;
}

template<class T>
node<T>* succ(node<T>* x){
  if(x){
    if(x->right) return min(x->right);
    //find an ancestor, whose left child contains x
    node<T>* p = x->parent;
    while(p && p->right==x){
      x = p;
      p = p->parent;
    }
    return p;
  }
  return 0;
}

template<class T>
node<T>* pred(node<T>* x){
  if(x){
    if(x->left) return max(x->left);
    //find an ancestor, whose right child contains x
    node<T>* p = x->parent;
    while(p && p->left==x){
      x = p;
      p = p->parent;
    }
    return p;
  }
  return 0;
}

template<class T>
node<T>* insert(node<T>* tree, T key){
  node<T>* root(tree);
  node<T>* x = new node<T>(key);
  node<T>* parent(0);
  while(tree){
    parent = tree;
    if(key < tree->key)
      tree = tree -> left;
    else //assert there is no duplicated key inserted.
      tree = tree -> right;
  }
  x->parent = parent;
  if( parent == 0 ) //tree is empty
    return x;
  else if( key < parent->key)
    parent->left = x;
  else
    parent->right = x;
  return root;
}

// cut the node off the tree, then delete it.
// it can prevent dtor removed children of a node
template<class T>
void remove_node(node<T>* x){
  if(x)
    x->left = x->right = 0;
  delete x;
}

// The algorithm described in CLRS isn't used here.
// I used the algorithm as below (refer to Annotated STL, P 235 (by Hou Jie)
//   if x has only one child: just splice x out
//   if x has two children: use min(right) to replace x
// @return root of the tree
template<class T>
node<T>* del(node<T>* tree, node<T>* x){
  if(!x)
    return tree;

  node<T>* root(tree);
  node<T>* old_x(x);
  node<T>* parent(x->parent);

  if(x->left == 0)
    x = x->right;
  else if(x->right == 0)
    x = x->left;
  else{
    node<T>* y=min(x->right);
    x->key = y->key;
    if(y->parent != x)
      y->parent->left = y->right;
    else
      x->right = y->right;

    remove_node(y);
    return root;
  }

  if(x)
    x->parent = parent;

  if(!parent)
    root = x; //remove node of a tree
  else
    if(parent->left == old_x)
      parent->left = x;
    else
      parent->right = x;

  remove_node(old_x);
  return root;
}

//for testing
template<class Coll>
node<typename Coll::value_type>* build_tree(const Coll& coll){
  node<typename Coll::value_type>* tree(0);
  for(typename Coll::const_iterator it=coll.begin(); it!=coll.end(); ++it)
    tree = insert(tree, *it);
  return tree;
}

template<class T>
std::string tree_to_str(const node<T>* tree){
  if(tree){
    std::ostringstream s;
    s<<"("<<tree_to_str(tree->left)<<"), "<<tree->key
     <<", ("<<tree_to_str(tree->right)<<")";
    return s.str();
  }
  return "empty";
}

template<class T>
node<T>* clone_tree(const node<T>* t, node<T>* parent=0){
  if(t){
    node<T>* t1 = new node<T>(t->key);
    t1->left = clone_tree(t->left, t1);
    t1->right = clone_tree(t->right, t1);
    t1->parent = parent;
    return t1;
  }
  return static_cast<node<T>*>(0);
}

//test helper

class test{
public:
  test(){
    const int buf[]={15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9};
    tree = build_tree(std::vector<int>(buf, buf+sizeof(buf)/sizeof(int)));
    std::cout<<tree_to_str(tree);
  }

  ~test(){
    delete tree;
  }

  template<class T> void assert_(std::string msg, T x, T y){
    std::cout<<msg;
    if(x==y)
      std::cout<<x<<" OK.\n";
    else
      std::cout<<x<<"!="<<y<<" Fail.\n";
  }

  void run(){
    test_in_order_walk();
    test_min_max();
    test_search();
    test_succ_pred();
    test_del();
  }

private:

  struct Print{
    template<class T>
    void operator()(T x){ std::cout<<x<<", "; }
  };

  void test_in_order_walk(){
    std::cout<<"\ntest in order walk with print functor: ";
    in_order_walk(tree, Print());
    //this can be simplified by using boost
    //using namespace boost::lambda;
    //in_order_walk(tree, std::cout<<_1<<", ");
  }

  void test_min_max(){
    node<int>* empty(0);
    assert_("min(empty)=", min(empty), empty);
    assert_("min(tree)=", min(tree)->key, 2);
    assert_("max(empty)=",max(empty), empty);
    assert_("max(tree)=", max(tree)->key, 20);
  }
  
  void test_search(){
    node<int>* empty(0);
    assert_("search empty: ", search(empty, 3), empty);
    std::cout<<"search exist key: "<<tree_to_str(search(tree, 18))<<"\n";
    assert_("search non-exist: ", search(tree, 5), empty);
  }

  void test_succ_pred(){
    node<int>* empty(0);
    assert_("succ 7: ", succ(search(tree, 7))->key, 9);
    assert_("succ 13: ", succ(search(tree, 13))->key, 15);
    assert_("succ 20: ", succ(search(tree, 20)), empty);
    assert_("pred 6: ", pred(search(tree, 6))->key, 4);
    assert_("pred 7: ", pred(search(tree, 7))->key, 6);
    assert_("pred 2: ", pred(search(tree, 2)), empty);
  }

  void test_del_n(int n){
    node<int>* empty(0);
    node<int>* t1=clone_tree(tree);
    t1=del(t1, search(t1, n));
    std::cout<<"del "<<n<<":\n"<<tree_to_str(t1)<<"\n";
    assert_("search after del: ", search(t1, n), empty);
    delete t1;
  }

  void test_del(){
    test_del_n(17);
    test_del_n(7);
    test_del_n(6);
    test_del_n(15);
    test_del_n(1); //try to del a non-exist val
  }
private:
  node<int>* tree;
};

int main(int, char**){
  test().run();
}

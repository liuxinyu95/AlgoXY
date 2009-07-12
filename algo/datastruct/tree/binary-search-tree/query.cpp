#include <iostream>
#include <vector>

template<class T>
struct node{
  node(T x):value(x), left(0), right(0), parent(0){}
  ~node(){
    if(left)
      delete left;
    if(right)
      delete right;
  }

  node* left; 
  node* right;
  node* parent;
  T value;
};

template<class T>
node<T>* min(node<T>* x){
  while(x->left)
    x = x->left;
  return x;
}

template<class T>
node<T>* insert(node<T>* tree, T value){
  node<T>* x = new node<T>(value);
  node<T>* parent(0);
  while(tree){
    parent = tree;
    if(value < tree->value)
      tree = tree -> left;
    else //assert there is no duplicated value inserted.
      tree = tree -> right;
  }
  x->parent = parent;
  if( parent == 0 ) //teee is empty
    return x;
  else if( value < parent->value)
    parent->left = x;
  else
    parent->right = x;
}

/*node& succ(node& x){
  }*/

//for testing
template<class Coll>
node<typename Coll::value_type>* build_tree(const Coll& coll){
  node<typename Coll::value_type>* tree(0);
  for(typename Coll::const_iterator it=coll.begin(); it!=coll.end(); ++it)
    tree = insert(tree, *it);
  return tree;
}

template<class T>
void print(const node<T>* tree){
  if(tree){
    print(tree->left);
    std::cout<<tree->value<<", ";
    print(tree->right);
  }
}

int main(int, char**){
  const int buf[]={15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9};
  node<int>* tree = build_tree(std::vector<int>(buf, buf+sizeof(buf)/sizeof(int)));
  print(tree);
  std::cout<<"\nmin(tree)="<<min(tree)->value<<"\n";
  delete tree;
}

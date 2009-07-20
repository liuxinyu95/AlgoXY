#include <iostream>
#include <vector>

template<class T>
struct node{
  node(T x):value(x), left(0), right(0), parent(0){}
  ~node(){ // for convinient, use functional approach
    if(left)
      delete left;
    if(right)
      delete right;
  }

  node* left; 
  node* right;
  node* parent; //parent is optional, it's helpful for succ/pred
  T value;
};

// in-order tree walk
// easy implemented by using functional approach
template<class T, class F>
void in_order_walk(node<T>* t, F f){
  if(t){
    in_order_walk(t->left, f);
    f(t->value);
    in_order_walk(t->right, f);
  }
}

template<class T>
node<T>* search(node<T>* t, T x){
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
    std::cout<<"(";
    print(tree->left);
    std::cout<<"), "<<tree->value<<", (";
    print(tree->right);
    std::cout<<")";
  }
  else
    std::cout<<"empty";
}

//test helper

class test{
public:
  test(){
    const int buf[]={15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9};
    tree = build_tree(std::vector<int>(buf, buf+sizeof(buf)/sizeof(int)));
    print(tree);
  }

  ~test(){
    delete tree;
  }

  void run(){
    test_in_order_walk();
    test_min_max();
  }

private:

  struct Print{
    template<class T>
    void operator()(T x){ std::cout<<x<<", "; }
  };

  void test_in_order_walk(){
    std::cout<<"\ntest in order walk with print functor: ";
    in_order_walk(tree, Print());
  }

  void test_min_max(){
    node<int>* empty(0);
    std::cout<<"\nmin(empty)="<<min(empty)
             <<"\nmin(tree)="<<min(tree)->value
             <<"\nmax(empty)="<<max(empty)
             <<"\nmax(tree)="<<max(tree)->value<<"\n";
  }
  
private:
  node<int>* tree;
};

int main(int, char**){
  test().run();
}

#include <iostream>
#include <vector>
#include <list>
#include <algorithm>
#include <iterator>
#include <functional>
#include <numeric>

using namespace std;

typedef vector<int> NumberList;
typedef vector<NumberList> Result;

//
// next 2 functions, permutation() and generate are typical implementation
// of permutation.
//
void generate(Result& res, const NumberList& inst, int i, int n, int r){
  for(int j=1; j<=n; ++j){
    if(find(inst.begin(), inst.end(), j)==inst.end()){
      NumberList new_inst(inst);
      new_inst.push_back(j);
      if(i==r)
        res.push_back(new_inst);
      else
        generate(res, new_inst, i+1, n, r);
    }
  }
}

Result permutation(int n, int r){
  Result res;
  generate(res, NumberList(), 1, n, r);
  return res;
}

// 
// Print, generic functor for printing 2D collections
// 

struct Print{
  template<typename Container>
  void operator()(Container& item){
    typedef typename Container::value_type value_type;
    copy(item.begin(), item.end(), ostream_iterator<value_type>(cout, " "));
    cout<<"\n";
  }
};

void test_permutation(){
  Result res=permutation(5, 3);
  for_each(res.begin(), res.end(), Print());
}

void test_std_permutation(){
  string str("abcdef");
  do{
    cout<<str<<"\n";
  }
  while(next_permutation(str.begin(), str.end()));
}

// 
// method1, use permutation + filter to solve the problem
// 

template<typename Compare>
bool checkOrder(Compare op, char c, const string& candidate){
    return op(candidate.find(c), candidate.find(c+1));
}

bool hasPattern(string& pattern, const string& candidate){
  char c='a';
  for(string::iterator it=pattern.begin(); it!=pattern.end(); ++it, ++c){
    if(*it=='A' && !checkOrder(less<char>(), c, candidate))
      return false;
    if(*it=='B' && !checkOrder(greater<char>(), c, candidate))
      return false;
  }
  return true;
}

void enumStr(string pattern){
  string s;
  for(int i=0; i<pattern.length()+1;++i)
    s.push_back('a'+i);
  vector<string> res;
  do{
    if(hasPattern(pattern, s))
      res.push_back(s);
  }while(next_permutation(s.begin(), s.end()));
  for_each(res.begin(), res.end(), Print());
  cout<<"total solution: "<<res.size()<<"\n";
}

void test_enumStr(){
    enumStr("AAABBA");
}

// 
// method 2, solve the problem by generate valid strings
// 

typedef list<string> ResultList;

ResultList insertChar(string inst, char c){
  ResultList res;
  typedef string::size_type size_type;
  size_type pos=inst.find(*max_element(inst.begin(), inst.end()));
  size_type from= (c=='A')? pos+1 : 0;
  size_type to  = (c=='A')? inst.length() : pos;

  for(size_type i=from; i<=to; ++i){
    string s(inst);
    s.insert(i, 1, char(inst[pos]+1));
    res.push_back(s);
  }
  return res;
}

ResultList enumStr2(string pattern){
  ResultList res;
  res.push_back(string("a"));
  for(string::iterator it=pattern.begin(); it!=pattern.end(); ++it){
    ResultList new_res;
    for(list<string>::iterator inst=res.begin(); inst!=res.end(); ++inst){
      ResultList s=insertChar(*inst, *it);
      copy(s.begin(), s.end(), 
           insert_iterator<ResultList>(new_res, new_res.end()));
    }
    res=new_res;
  }
  return res;
}

void test_enumStr2(){
  ResultList res=enumStr2("AAABBA");
  for_each(res.begin(), res.end(), Print());
  cout<<"total solution:"<<res.size()<<"\n";
}

//
// method 3, solve the problem without enumerate all strings
// 

template<typename Coll>
void insertRange(int from, int to, Coll& res){
    int step = from<to ? 1 : -1;
    for(;;from+=step){
        res.push_back(from);
        if(from==to)
          break;
    }
}

int enumStr3(string pattern){
    list<int> res(1,1);
    for(int i=0; i<pattern.length(); ++i){
        list<int> new_res;
        if(i && pattern[i]!=pattern[i-1])
            for(list<int>::iterator node=res.begin(); node!=res.end(); ++node)
                insertRange(i+1, i-*node+2, new_res);
        else
            for(list<int>::iterator node=res.begin(); node!=res.end(); ++node)
                insertRange(1, *node, new_res);
        res=new_res;    
    }
    return accumulate(res.begin(), res.end(), 0);
}

void test_enumStr3(){
    cout<<"enum A="<<enumStr3("A")<<"\n"
          <<"enum AA="<<enumStr3("AA")<<"\n"
        <<"enum AAABBA="<<enumStr3("AAABBA")<<"\n";
}

int main(int, char**){
  //test_permutation();
  //test_std_permutation();
  //test_enumStr();
  //test_enumStr2();
    test_enumStr3();
}

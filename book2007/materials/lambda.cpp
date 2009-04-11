#include <cmath>
#include <iostream>
#include <vector>
#include <cstdlib>

#include "lambda.h"

void testBasic(){
	//test _x(1, 2) = 1;
	std::cout<<"_x(1,2)="<<_x(1,2)<<std::endl;

	//test _y(1, 2) = 2;
	std::cout<<"_y(1,2)="<<_y(1,2)<<std::endl;

	//test _x(1)=1;
	std::cout<<"_x(1)="<<_x(1)<<std::endl;

	//test plus(1,2)() = 3;
	std::cout<<"plus(1, 2)()="<<plus(1, 2)()<<std::endl;

	//test plus(_x, _y)(3, 4) = 7;
	std::cout<<"plus(_x, _y)(3, 4)="
		<<plus(_x, _y)(3, 4)<<std::endl;

	//test Find<Premiers, int>::value
	std::cout<<"Find<Premiers, int>::value="
		<<Find<Premiers, int>::value<<std::endl;

	//test plus(_x, 3)(2) = 2+3 = 5;
	std::cout<<"plus<_x, 3)(2)="
		<<plus(_x, 3)(2)<<std::endl;

	//test plus(_x, plus(_x, 2)(1)= 1 + 1 + 2 = 4;
	std::cout<<"plus(_x, plus(_x, 2)(1)="
		<<plus(_x, plus(_x, 2))(1)<<std::endl;
}

//
// high order sum test driver
//
template<class T, class F>
T sum_lambda(T a, T b, F f){
	T res(0);
	for(T i=a; i<=b; ++i)
		res+=f(i);
	return res;
}

void testArithmetic(){
	std::cout<<"[f(x)=2*(x+1)], f(1)+f(2)+...+f(100)="
		<<sum_lambda(1, 100, lambda(arg(_x), body(multiply(2, plus(_x, 1)))))<<"\n";
		
	std::cout<<"[f(x)=(x+1)*(x-1)], f(1)+f(2)+...+f(100)="
		<<sum_lambda(1, 100, lambda(arg(_x), body(multiply(plus(_x, 1), minus(_x, 1)))))<<"\n";
		
	std::cout<<"[f(x)=(x-1.0)/2.0], f(1)+f(2)+...+f(100)="
		<<sum_lambda(1.0, 100.0, lambda(arg(_x), body(divide(minus(_x, 1.0), 2.0))))<<"\n";
}

//
// if-then-else test driver
//
template<class T, class F>
T trans(T coll, F f){
	T res;
	for(typename T::iterator it=coll.begin(); it!=coll.end(); ++it){
		res.push_back(f(*it));
	}
	return res;
}

void testIfThenElse(){
	std::cout<<"if(x/2==0)then(1).else(0)\n";
	std::vector<int> coll;
	for(int i=0; i<10; ++i)
		coll.push_back(rand());
	
	std::vector<int> res = trans(coll, 
		lambda(arg(_x), body(
			If(eq(multiply(divide(_x, 2), 2), _x)).Then(1).Else(0))));
	
	for(int i=0; i<10; ++i)
		std::cout<<"even("<<coll[i]<<")="<<res[i]<<",  ";
	std::cout<<'\n';
}

int main(int argc, char** argv){
	std::cout<<"============test lambda=========\n";
	testBasic();
	testArithmetic();
	testIfThenElse();
}


#ifndef __LAMBDA__
#define __LAMBDA__

#include "testcase.h"
#include "testscript.h"
#include "testcaller.h"
#include "asserttool.h"
/*
define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))


(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
*/

int sumInt(int a, int b){
	int res=0;
	for(int i=a; i<=b; ++i)
		res+=i;
	return res;
}

int sumCube(int a, int b){
	int res=0;
	for(int i=a; i<=b; ++i)
		res+= i*i*i;
	return res;
}

double sumPi(int a, int b){
	double res=0;
	for(int i=1; i<=b; i+=4)
		res+= 1/(static_cast<double>(i)*static_cast<double>(i+2));
	return res;
}

class TestLambda : public TestCase
{
	void test(){
		TestScript<TestLambda>::begin(this)
			<<&TestLambda::testRun
			<<end;
	}

	void testRun(){
		std::cout<<"=============test lambda run==============\n";
		std::cout<<"sum int 1..100="<<sumInt(1, 100)<<"\n";
		std::cout<<"sum cube 1..100="<<sumCube(1, 100)<<"\n";
		std::cout<<"sum to PI 1..100="<<8.0*sumPi(1, 100)<<"\n";
	}
};

#endif //__LAMBDA__

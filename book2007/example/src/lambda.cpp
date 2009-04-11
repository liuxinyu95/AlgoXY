#include "lambda_demo.h"
#include "lambda.h"

#include "test/testcase.h"
#include "test/testscript.h"
#include "test/testcaller.h"
#include "test/asserttool.h"
#include <cmath>

//
// high order sum test driver
//
template<class F>
int sum_lambda(int a, int b, F f){
	int res(0);
	for(int i=a; i<=b; ++i)
		res+=f(i);
	return res;
}

//
// main tests
//

class TestLambda : public TestCase
{
	void test(){
		TestScript<TestLambda>::begin(this)
			<<&TestLambda::testTrivial
			<<&TestLambda::testHighOrderSum
			<<&TestLambda::testStaticTrivial
			<<&TestLambda::testStaticHighOrderSum
			<<&TestLambda::testIntegral
			<<&TestLambda::testTransform
			<<&TestLambda::testLambdaBasic
			<<&TestLambda::testLambda
			<<end;
	}

	void testTrivial(){
		std::cout<<"=============trivial func calls==============\n";
		std::cout<<"sum int 1..100="<<sumInt(1, 100)<<"\n";
		std::cout<<"sum cube 1..100="<<sumCube(1, 100)<<"\n";
		std::cout<<"sum to PI 1..100="<<8.0*sumPi(1, 100)<<"\n";
	}

	void testHighOrderSum(){
		std::cout<<"=============high order sum calls==============\n";
		std::cout<<"sum int 1..100=" <<sumGeneric(1, 100, Self<int>(), Inc<int>())<<"\n";
		std::cout<<"sum cube 1..100="<<sumGeneric(1, 100, Cube<int>(), Inc<int>())<<"\n";
		std::cout<<"sum to PI 1..100="<<8.0*
									   sumGeneric(1.0, 100.0, MyFunc<double>(), Inc4<double>())<<"\n";

		std::cout<<"=============test increase==============\n";
		std::cout<<"sum int 1..100=" <<sumGeneric(1, 100, Self<int>(), Increase<int>())<<"\n";
		std::cout<<"sum cube 1..100="<<sumGeneric(1, 100, Cube<int>(), Increase<int>())<<"\n";
		std::cout<<"sum to PI 1..100="<<8.0*
									   sumGeneric(1.0, 100.0, MyFunc<double>(), Increase<double, 4>())<<"\n";
	}

	void testStaticTrivial(){
		std::cout<<"=============fp trivial func calls==============\n";
		std::cout<<"sum int 1..100="<<SumInt<1, 100>::value<<"\n";
		std::cout<<"sum cube 1..100="<<SumCube<1, 100>::value<<"\n";
		//std::cout<<"sum to speical 1..20="<<SumSpecial<1, 20>::value<<"\n";
	}

	void testStaticHighOrderSum(){
		std::cout<<"=============fp high order func calls==============\n";
		std::cout<<"sum int 1..100="<<SumGeneric<1, 100, SelfTrans>::value<<"\n";
		std::cout<<"sum cube 1..100="<<SumGeneric<1, 100, CubeTrans>::value<<"\n";
	}


	//this works for G++
	struct Sin{
		double operator()(double x){ return sin(x); }
	};

	void testIntegral(){
		std::cout<<"=============Integral==============\n";

		/* this only work in Visual C++ 2005
		struct Sin{
			double operator()(double x){ return sin(x); }
		};
		*/
		const double PI=3.1415927;

		std::cout<<"Int xdx (0~4)= 0.5x^2 (0~4)="<<integral(Self<double>(), 0.0, 4.0, 0.001)<<"\n";
		std::cout<<"Int x^3dx (0~4)= 0.25x^4 (0~4)="<<integral(Cube<double>(), 0.0, 4.0, 0.001)<<"\n";
		std::cout<<"Int sin(x)dx (0~pi/2)= "<<integral(Sin(), 0.0, PI/2, 0.001)<<"\n";
	}

	//this works for G++
	//F->C
	//C = (F-32)*100 / (212-32)
	struct FtoC{
		double operator()(double x){ return (x-32)*100/(212-32); }
	};

	//this works for g++
	//full name --> family name
	struct FamilyName{
		std::string operator()(std::string fullName){
			std::string::size_type idx = fullName.find(',');
			return fullName.substr(idx+2);
		}
	};
	void testTransform(){
		std::cout<<"=============high order transform=========\n";

		/*
		 * this only works for Visual C++ 2005
		//F->C
		//C = (F-32)*100 / (212-32)
		struct FtoC{
			double operator()(double x){ return (x-32)*100/(212-32); }
		};
		*/
		List<double>* flist = cons(200.0, cons(10.0, cons(85.0, cons(20.0, cons(15.0, cons(32.0, static_cast<List<double>*>(0)))))));
		List<double>* clist = transform(flist, FtoC());
		printList(clist);
		delete clist;
		delete flist;

		/*
		 * this only works for Visual C++ 2005
		//full name --> family name
		struct FamilyName{
			std::string operator()(std::string fullName){
				std::string::size_type idx = fullName.find(',');
				return fullName.substr(idx+2);
			}
		};
		*/
		List<std::string>* namelist = cons(std::string("Bill, Gates"), 
			cons(std::string("Steve, Jobs"),
			cons(std::string("Linus, Torvalds"), 
			cons(std::string("Larry, Wall"), static_cast<List<std::string>*>(0)))));
		List<std::string>* familylist = transform(namelist, FamilyName());
		printList(familylist);
		delete namelist;
		delete familylist;
	}

	void testLambdaBasic(){
		std::cout<<"=============lambda=========\n";
		std::cout<<"test lambda\n";

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

	void testLambda(){
		std::cout<<"[f(x)=2*(x+1)], f(1)+f(2)+...+f(100)="
			<<sum_lambda(1, 100, lambda(arg(_x), body(multiply(2, plus(_x, 1)))))<<"\n";
	}
};

namespace{
	//const bool res=TestSuite::instance().add(new TestLambda);
}

#ifndef __TEST_CALLER__
#define __TEST_CALLER__

#include <string>
#include <iostream>

template<class T>
class TestCaller: public TestCase{
public:
	typedef void (T::*TestMethod)();

	TestCaller(T* test, TestMethod func):testCase(test), func(func){
		//testCase = new T;
	}

	~TestCaller(){
		//delete testCase;
	}

	void test(){
		(testCase->*func)();
	}

private:
	T* testCase;
	TestMethod func;
};

#endif
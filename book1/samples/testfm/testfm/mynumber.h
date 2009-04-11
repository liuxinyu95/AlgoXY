#ifndef __MY_NUMBER__
#define __MY_NUMBER__

#include <iostream>
#include <sstream>
#include "testsuite.h"
#include "testcaller.h"
#include "testscript.h"

class MyNumber{
public:
	MyNumber(int x):value_(x){}
	MyNumber(const MyNumber& x):value_(x.value_){};

	const MyNumber& operator=(const MyNumber& x){
		value_=x.value_;
		return *this;
	}

	const MyNumber operator+(const MyNumber& x){
		MyNumber res(value_+x.value_);
		return res;
	}

	std::string asString(){
		std::stringstream ss;
		ss<<value_;
		return ss.str();
	}
private:
	int value_;
};

class TestMyNumber: public TestCase{
public:
	void test(){
		TestScript<TestMyNumber>::begin(this)
			<<&TestMyNumber::testInit
			<<&TestMyNumber::testAdd
			<<end;
		/*
		TEST_SCRIPT_BEGIN(TestMyNumber)
		TEST_SCRIPT_ADD(testInit)
		TEST_SCRIPT_ADD(testAdd)
		TEST_SCRIPT_END()
		*/
		/*
		TestSuite suite;
		suite.add(new TestCaller<TestMyNumber>(this, &TestMyNumber::testInit));
		suite.add(new TestCaller<TestMyNumber>(this, &TestMyNumber::testAdd));
		suite.run();
		*/
	}

	void testInit(){
		MyNumber three(3);
		assertTrue(three.asString() == "3");
	}

	void testAdd(){
		MyNumber three(3);
		MyNumber two(2);
		MyNumber res=three+two;
		assertTrue(res.asString() == "5");
	}
};

#endif //__MY_NUMBER__
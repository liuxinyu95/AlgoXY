#ifndef __MY_TIME__
#define __MY_TIME__

#include <iostream>
#include <string>
#include <sstream>
#include "testsuite.h"

class MyTime{
public:
	MyTime(int h, int m, int s):h_(h), m_(m), s_(s){}
	std::string asString(){
		std::stringstream ss;
		ss<<h_<<":"<<m_<<":"<<s_;
		return ss.str();
	}
private:
	int h_, m_, s_;
};

struct TestMyTime: public TestCase{
	void test(){
		MyTime tm1(9, 30, 12);
		assertTrue(tm1.asString()=="9:30:12");
		//other test scripts...
	}
};

#endif //#ifndef __MY_TIME__
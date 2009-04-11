#ifndef __TEST_SUITE__
#define __TEST_SUITE__

#include <vector>
#include "testcase.h"

class TestSuite{
public:
	static TestSuite& instance(){
		static TestSuite inst;
		return inst;
	}

	~TestSuite(){
		for(std::vector<TestCase*>::iterator it=tests.begin();
			it!=tests.end(); ++it)
			delete (*it);
	}

	bool add(TestCase* test){ tests.push_back(test); return true; }

	void run(){
		for(std::vector<TestCase*>::iterator it=tests.begin();
			it!=tests.end(); ++it)
				(*it)->test();
	}

private:
	std::vector<TestCase*> tests;
};

#endif
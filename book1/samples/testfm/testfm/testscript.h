#ifndef __TEST_SCRIPT__
#define __TEST_SCRIPT__

#include "testsuite.h"

template<class T>
class TestScript{
public:
	typedef T _CaseType;

	static TestScript& begin(_CaseType* test){
		TestScript* self=new TestScript(test);
		return *self;
	}

	TestScript(_CaseType* test): testCase(test){};

	template<class MemFunc>
	TestScript& operator<<(MemFunc func){
		suite.add(new TestCaller<_CaseType>(testCase, func));
		return *this;
	}

	typedef void (*_Manipulator)(TestScript&);
	void operator<<(_Manipulator op){
		op(*this);
	}

	void suicide(){ delete this; }

	~TestScript(){
		suite.run();
	}

private:
	TestSuite suite;
	_CaseType* testCase;
};

template<class T>
inline void end(TestScript<T>& self){
	self.suicide();
}

#endif

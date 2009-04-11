#ifndef __TEST_CASE__
#define __TEST_CASE__

#include <string>
#include "asserttool.h"

#define TEST_SCRIPT_BEGIN(CaseType) \
	{\
		typedef CaseType _CaseType; \
		TestSuite suite;\

#define TEST_SCRIPT_ADD(func)	\
	suite.add(new TestCaller<_CaseType>(this, &TestMyNumber::func));

#define TEST_SCRIPT_END() \
	suite.run(); \
	} \

//	suite.add(new TestCaller<TestMyNumber>(this, &TestMyNumber::testInit));

class TestCase{
public:
	virtual void test()=0;
	virtual ~TestCase(){};
};

#endif
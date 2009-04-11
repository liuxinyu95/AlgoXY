#include "reflect.h"

//IMPLEMENT_REFLECTIVE(Lion)
namespace{
	const bool res=TestSuite::instance().add(new TestReflection);
}
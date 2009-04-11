#include "lambda.h"

#include "test/testcase.h"
#include "test/testscript.h"
#include "test/testcaller.h"
#include "test/asserttool.h"

class point{
public:
	point(int ax, int ay):x(ax), y(ay){}
	const bool operator==(const point& p){
		return get_x() == p.get_x() && 
		       get_y() == p.get_y();
	}
	
	const int get_x() const { return x; }
	const int get_y() const { return y; }
private:
	int x;
	int y;
};

class color_point{
public:
	enum { r, g, b};
	color_point(int x, int y, int c):_p(x, y), color(c){}

	const int get_x() const { return _p.get_x(); }
	const int get_y() const { return _p.get_y(); }
private:
	point _p;
	int color;
};

template<class T, class U>
const bool is_equal_point(T p1, U p2){ //const T& p1, const U& p2 doesn't work!
	return p1.get_x() == p2.get_x() &&
	       p1.get_y() == p2.get_y();
}

template<int x, int y>
struct Point{
	const int get_x() const { return x; }
	const int get_y() const { return y; }
};

template<int x, int y>
struct PointLambda{
#define __f lambda(arg(_x), body(If(eq(_x, 0)).Then(x).Else(y)))

	const int get_x() const { return __f(0); }
	const int get_y() const { return __f(1); }
};

struct CreatePoint{
	CreatePoint(int x, int y){
		f=lambda(arg(_x), body(If(eq(_x, 0)).Then(x).Else(y)));
	}
	CreatePoint(const CreatePoint& ref):f(ref.f){}
	const int get_x() { return f(0); }
	const int get_y() { return f(1); }
private:
	typedef IfOp<BinaryOp<Var<1>, int, Eq>, int, int> FunctionType;
	FunctionType f;
};

//
// PointFunction::get_x(create_point(1, 2)) == 1;
// get_y(create_point(1, 2)) == 2;
//

class TestEquality : public TestCase
{
	void test(){
		TestScript<TestEquality>::begin(this)
			<<&TestEquality::testEqual
			<<end;
	}

	void testEqual(){
		color_point p3(1, 2, color_point::r);
		point p4(1, 2);

		if( is_equal_point(p3, p4) )
			std::cout<<"p3 is equal to p4.\n";
		else
			std::cout<<"p3 and p4 are not equal.\n";

		if( is_equal_point(p4, Point<1, 2>()) )
			std::cout<<"p4 is equal to Point<1, 2>().\n";

		if( is_equal_point(p4, PointLambda<1, 2>()) )
			std::cout<<"p4 is equal to PointLambda<1, 2>().\n";

		if( is_equal_point(p4, CreatePoint(1, 2)) )
			std::cout<<"p4 is equal to CreatePoint(1, 2).\n";
	}
};

namespace{
	//const bool res=TestSuite::instance().add(new TestEquality);
}

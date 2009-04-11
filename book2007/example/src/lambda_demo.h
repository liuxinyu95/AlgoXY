#ifndef __LAMBDA_DEMO__
#define __LAMBDA_DEMO__

#include <iostream>

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

//
// trivial sum functions
//
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

//
// high order sum funciton
//
template<class T, class F, class G>
T sumGeneric(T a, T b, F func, G next){
	T res(0);
	for(T i=a; i<=b; next(i))
		res += func(i);
	return res;
};

//
// trivial functors
//
template<class T>
struct Self{
	T operator()(T x) { return x; }
};

template<class T>
struct Cube{
	T operator()(T x) { return x*x*x; }
};

template<class T>
struct MyFunc{
	T operator()(T x){
		return 1/(x*(x+2));
	}
};

template<class T>
struct Inc{
	void operator()(T& x){ ++x;}
};

template<class T>
struct Inc4{
	void operator()(T& x){ x+=4;}
};

template<class T, int n=1>
struct Increase{
	void operator()(T& x){ x+=n; }
};

//
// fp sum
//
template<int a, int b>
struct SumInt{
	static const int value = a + SumInt< a+1, b>::value;
};

template<int b>
struct SumInt<b, b>{ static const int value = b; };

template<int a, int b>
struct SumCube{
	static const int value = a*a*a + SumCube< a+1, b>::value;
};

template<int b>
struct SumCube<b, b>{ static const int value = b*b*b; };

//
// gp sum
//

template<int a, int b, template<int> class Func>
struct SumGeneric{
	static const int value = Func<a>::value + SumGeneric< a+1, b, Func>::value;
};

template<int b, template<int> class Func>
struct SumGeneric<b, b, Func>{ static const int value = Func<b>::value; };

template<int x>
struct SelfTrans{
	static const int value = x;
};

template<int x>
struct CubeTrans{
	static const int value = x*x*x;
};

//
// Integral
//

//this works for g++
struct Inc_dx{
	Inc_dx(double dx):_dx(dx){}
	void operator()(double& x){
		x+=_dx;
	}
private:
	double _dx;
};

template<class Func>
double integral(Func f, double a, double b, double dx){
	/*
	 * this only works for Visual C++ 2005
	struct Inc_dx{
		Inc_dx(double dx):_dx(dx){}
		void operator()(double& x){
			x+=_dx;
		}
	private:
		double _dx;
	};
	*/
	return dx*sumGeneric(a, b, f, Inc_dx(dx));
}

//
// Transform
//
template<class List, class Func>
List* transform(List* alist, Func f){
	if(!alist)
		return 0;	//null
	else
		return cons( f(alist->first), transform(alist->rest, f) );
}

//
// List
//
template<class T>
struct List{
	List(const T& x, List<T>* y): first(x), rest(y){}
	~List(){ delete rest; }

	T first;
	List* rest;
};

template<class T>
List<T>* cons(const T& x, List<T>* y){
	return new List<T>(x, y);
}

template<class List>
void printList(List* alist){
	if(alist == 0)
		std::cout<<"\n";
	else{
		std::cout<<alist->first<<", ";
		printList(alist->rest);
	}
};

#endif

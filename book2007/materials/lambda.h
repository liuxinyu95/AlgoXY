#ifndef __LAMBDA__
#define __LAMBDA__

//
// assumption:
// 1. return type = arg type
//

struct Empty;

//
// type list
//

template<class T, class U>
struct TList{
	typedef T First;
	typedef U Rest;
};

//
// Append 
//

template<class List1, class List2> struct Append{
	typedef TList<
		typename List1::First,
		typename Append<typename List1::Rest, List2>::Result> Result;
};

template<class List2> struct Append<Empty, List2>{
	typedef List2 Result;
};

//
// Premiers types
//

typedef TList<int, 
	TList<long, 
		TList<short, 
			TList<char, 
				TList<long long, Empty> > > > > Integers;

typedef TList<float, TList<double, Empty> > Reals;

typedef Append<Integers, Reals>::Result Premiers;

//
// Find type
//

template<class TypeList, class T> struct Find{
    static const int value = 1 + Find<typename TypeList::Rest, T>::value;
};

template<class T, class U> struct Find<TList<T, U>, T>{
    static const int value = 0;
};

template<class T> struct Find<Empty, T>{
    static const int value = -1000;
};

//
// is functor or premier?
//

template<class Func> struct IsFunctor{
    static const bool value = Find<Premiers, Func>::value < 0;
};

//
// eval()
//
template<bool isFunctor> struct Eval;

// f is a functor
template<> struct Eval<true>{
    template<class F, class T>
    static T apply(F f, T x){
        return f(x);
    }

    template<class F, class T>
    static T apply(F f, T x1, T x2){
        return f(x1, x2);
    }
};

// f is not a functor
//  ==>f is const premier value;
template<> struct Eval<false>{
    template<class F, class T>
    static T apply(F f, T x){
        return f;
    }
};

//
// Projectors
//

template<int n> struct Var;

template<> struct Var<1>{
    template<class T>
    T operator()(T a1, T a2){ return a1; }

    template<class T>
    T operator()(T a1){ return a1; }
};

template<> struct Var<2>{
  template<class T>
  T operator()(T a1, T a2){ return a2; }
};

//
// helpers
//

#define _x Var<1>()
#define _y Var<2>()

//
// BinaryOp
//

template<class Func1, class Func2, class Op>
struct BinaryOp{
	BinaryOp(){}
    BinaryOp(Func1 f1, Func2 f2):_f1(f1),_f2(f2){}

    // ex: f=op(_x, _y), f(1, 2)
    template<class T>
    T operator()(T x1, T x2){
		T x = IsFunctor<Func1>::value ? _f1(x1, x2) : x1;
		T y = IsFunctor<Func2>::value ? _f2(x1, x2) : x2;
		return Op::proc(x, y);
    }

    // ex: f=op(_x, 1), f(2)
    template<class T>
    T operator()(T x1){
		T x = Eval<IsFunctor<Func1>::value>::apply(_f1, x1);
		T y = Eval<IsFunctor<Func2>::value>::apply(_f2, x1);
		return Op::proc(x, y);
    }

    // ex: f=op(1, 2), f()
    //means that _f1 & f2 are premiers
    Func1 operator()(){
		return Op::proc(_f1, _f2);
    }

private:
    Func1 _f1;
    Func2 _f2;
};

//
// concrete binary Ops
//

struct Plus{
    template<class T>
    static T proc(T x, T y){ return x+y; }
};

template<class T, class U>
BinaryOp<T, U, Plus> plus(T x, U y){
    return BinaryOp<T, U, Plus>(x, y);
}

struct Minus{
    template<class T> static T proc(T x, T y){ return x-y; }
};

template<class T, class U>
BinaryOp<T, U, Minus> minus(T x, U y){
    return BinaryOp<T, U, Minus>(x, y);
}

struct Multiply{
  template<class T> static T proc(T x, T y){ return x*y; }
};

template<class T, class U>
BinaryOp<T, U, Multiply> multiply(T x, U y){
    return BinaryOp<T, U, Multiply>(x, y);
}

struct Divide{
  template<class T> static T proc(T x, T y){ return x/y; }
};

template<class T, class U>
BinaryOp<T, U, Divide> divide(T x, U y){
    return BinaryOp<T, U, Divide>(x, y);
}

//
// Eq
//
struct Eq{
  template<class T> static T proc(T x, T y){ return x==y; }
};

template<class T, class U>
BinaryOp<T, U, Eq> eq(T x, U y){
    return BinaryOp<T, U, Eq>(x, y);
}

//
// If - else
//

template<class Predicate, class Consequent, class Alternative>
struct IfOp{
	IfOp(){}
    IfOp(Predicate p, Consequent c, Alternative a): predic(p), conseq(c), alter(a){}
	IfOp(const IfOp& ref): predic(ref.predic), conseq(ref.conseq), alter(ref.alter){}

    // ex: f=If(_x==0, _y, _z), f(0, 1, -1) //==1

    // ex: f=If(_x==-1, 0, 1), f(-1) //==0
    template<class T>
    T operator()(T x1){
		int test = Eval<IsFunctor<Predicate>::value>::apply(predic, x1);
		if(test)
			return Eval<IsFunctor<Consequent>::value>::apply(conseq, x1);
		else
			return Eval<IsFunctor<Alternative>::value>::apply(alter, x1);
    }

private:
    Predicate  predic;
    Consequent conseq;
	Alternative alter;
};

//
// if else helper
// If(_x==1).Then(0).Else(_y)
//

template<class Predicate, class Consequent>
struct ThenHelper{
	ThenHelper(Predicate p, Consequent c): predic(p), conseq(c){}

	template<class T>
	IfOp<Predicate, Consequent, T> Else(T x){
		return IfOp<Predicate, Consequent, T>(predic, conseq, x);
	}

	Predicate predic;
	Consequent conseq;
};

template<class Predicate>
struct IfHelper{
	IfHelper(Predicate p):predic(p){}

	template<class T>
	ThenHelper<Predicate, T> Then(T x){
		return ThenHelper<Predicate, T>(predic, x);
	}

	Predicate predic;
};

template<class T>
IfHelper<T> If(T x){
	return IfHelper<T>(x);
}

//
// Lambda syntax wrapper
//

template<class A1, class A2>
int arg(A1 a1, A2 a2){ return 0; }

template<class A1>
int arg(A1 a1){ return 0; }

template<class T>
T body(T f){ return f; }

template<class T>
T lambda(int /*arg*/, T f){ return f;}


#endif //__LAMBDA__

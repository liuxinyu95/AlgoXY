#ifndef _FUNC_PROG_
#define _FUNC_PROG_

#include "testcase.h"
#include "testscript.h"
#include "testcaller.h"

// definition of TypeList
template<class T, class U>
struct TypeList{
	typedef T HEAD;
	typedef U TAIL;
	typedef HEAD First;
	typedef TAIL Rest;
};

struct NullType;

template<int x, int y>
struct Add{
	static const int value=x+y;
};

template<int x, int y>
struct Sub{
	static const int value=x-y;
};

template<int x, int y>
struct Multiply{
	static const int value=x*y;
};

template<int x, int y>
struct Division{
	static const int value=x/y;
};

template<int F>
struct Fah2Cel{
	//C = (F-32)*100/(312-32);
	static const int value = 
		Division<
			Multiply<Sub<F, 32>::value, 100>::value,
			Sub<312, 32>::value
				>::value;
};

const int P = 76;

template<int x, int y>
struct Pos{
	static const int x=x;
	static const int y=y;
};

template<class P1, class P2>
struct Product{
	static const int value = 
		Add<
			Sub<P1::x, P2::x>::value * Sub<P1::x, P2::x>::value,
			Sub<P1::y, P2::y>::value * Sub<P1::y, P2::y>::value
		>::value;
};

template<int n> struct IsEven{
	static const bool value = !(n%2);
};

template<bool flag> struct EvenOdd;
template<> struct EvenOdd<true>{
	static void Print(){ std::cout<<"Even\n"; }
};

template<> struct EvenOdd<false>{
	static void Print(){ std::cout<<"Odd\n"; }
};

template<int n> struct Discount{
	template<bool b1, bool b2, bool b3> struct Branch{
		static const int value = n;
	};

	template<> struct Branch< true, false, false >{
		static const int value = n*8/10;
	};

	template<> struct Branch< false, true, false>{
		static const int value = n*7/10;
	};

	template<> struct Branch< false, false, true>{
		static const int value = n*7/10 - 50;
	};

	static const int value = Branch<
		(n>=100 && n<500),
		(n>=500 && n<1000),
		(n>1000)
		>::value;
};

struct Empty;

template<int n, class T>
struct List{
	static const int First = n;
	typedef T Rest;
};

template<class NList> struct Sum;

template<> struct Sum<Empty>{
	static const int value = 0;
};

template<class NList> struct Sum{
	static const int value = NList::First + Sum<NList::Rest>::value;
};


template<int n> struct Factorial;

template<> struct Factorial<0>{
	static const int value = 1;
};

template<int n> struct Factorial{
	static const int value = n*Factorial<n-1>::value;
};

template<class NList> struct Pick;

template<> struct Pick<Empty>{
	typedef Empty Result;
};

/*
template<int n, class T> struct Pick<List<n, T> >{
	template<bool x> struct Branch;
	template<> struct Branch<true>{
		typedef List<n, typename Pick<T>::Result> Result;
	};

	template<> struct Branch<false>{
		typedef typename Pick<T>::Result Result;
	};

	typedef typename Branch<(n<20)>::Result Result;
};
*/
template<class NList> struct Pick{
	template<bool x> struct Branch;
	template<> struct Branch<true>{
		typedef List<NList::First, 
			typename Pick<typename NList::Rest>::Result> Result;
	};

	template<> struct Branch<false>{
		typedef typename Pick<typename NList::Rest>::Result Result;
	};

	typedef typename Branch< (NList::First)<20 >::Result Result;
};

template<class NList> struct Print;

template<int n, class T> struct Print<List<n, T> >{
	static void print(){
		std::cout<<n<<", ";
		Print<T>::print();
	}
};

template<> struct Print<Empty>{
	static void print(){ std::cout<<"\n"; }
};

template<class T, int n, class U> struct Tree{
	typedef T Left;
	static const int value = n;
	typedef U Right;
};

template<class NTree> struct PrintTree{
	static void print(){
		PrintTree<typename NTree::Left>::print();
		std::cout<<NTree::value<<", ";
		PrintTree<typename NTree::Right>::print();
	}
};

template<> struct PrintTree<Empty>{
	static void print(){}
};

template<class NList, int n> struct Append{
	typedef List<NList::First,
		typename Append<typename NList::Rest, n>::Result> Result;
};

template<int n> struct Append<Empty, n>{
	typedef List<n, Empty> Result;
};

template<int n> struct BuildList{
	typedef typename Append<
		typename BuildList<n-1>::Result, n>::Result Result;
};

template<> struct BuildList<2>{
	typedef List<2, Empty> Result;
};

template<class NList, int p> struct SieveList{
	template<bool x> struct If;
	template<> struct If<true>{
		typedef typename SieveList<typename NList::Rest, p>::Result Result;
	};
	template<> struct If<false>{
		typedef List< NList::First,
			typename SieveList<typename NList::Rest, p>::Result> Result;
	};

	typedef typename If< (NList::First % p) ==0 >::Result Result;
};

template<int p> struct SieveList<Empty, p>{
	typedef Empty Result;
};

// N --> listof (Prime Numbers)
// Input N, output a list of Prime Numbers.
template<int N> struct Sieve{
	template<class NList> struct SieveAll{
		typedef List<
			NList::First,
			typename SieveAll<
				typename SieveList< typename NList::Rest, NList::First>::Result
			>::Result
		> Result;
	};

	template<> struct SieveAll<Empty>{
		typedef Empty Result;
	};

	typedef typename SieveAll<
		typename BuildList<N>::Result >::Result Result;
};


template<class NList> struct Reverse{
	typedef typename Append<
		typename Reverse<typename NList::Rest>::Result, 
		NList::First>::Result Result;
};

template<> struct Reverse<Empty>{
	typedef Empty Result;
};

template<class NList> struct SwapPair{
	typedef List<NList::Rest::First,
		List<NList::First, 
			typename SwapPair<typename NList::Rest::Rest>::Result
			>> Result;
};

// 
template<> struct SwapPair<Empty>{
	typedef Empty Result;
};

//
template<int n> struct SwapPair< List<n, Empty> >{
	typedef List<n, Empty> Result;
};

struct TestFuncProg: public TestCase{
	void test(){
		TestScript<TestFuncProg>::begin(this)
			<<&TestFuncProg::testFactorial
			<<&TestFuncProg::testPick
			<<&TestFuncProg::testBuildList
			<<&TestFuncProg::testSieveList
			<<&TestFuncProg::testSieve
			<<&TestFuncProg::testReverse
			<<&TestFuncProg::testSwapPair
			<<end;
	}

	void testFactorial(){
		std::cout<<"5!="<<Factorial<5>::value<<"\n";
	}

	void testPick(){
		typedef List<100, List<15, List<24, List<5, List<1, Empty>>>>> Prices;
		Print< Pick<Prices>::Result >::print();
	}

	void testBuildList(){
		Print< BuildList<10>::Result >::print();
	}

	void testSieveList(){
		std::cout<<"test Sieve list: ";
		Print< SieveList< 
			typename BuildList<10>::Result, 2>::Result >::print();
	}

	void testSieve(){
		std::cout<<"test E Sieve: ";
		Print< Sieve<20>::Result >::print();
	}

	void testReverse(){
		std::cout<<"test reverse: ";
		typedef List<1, List<2, List<3, List<4, List<5, Empty>>>>> TestList;
		Print< Reverse<TestList>::Result >::print();
	}

	void testSwapPair(){
		std::cout<<"test swap pair: ";
		typedef List<1, List<2, List<3, List<4, List<5, Empty>>>>> TestList;
		Print< SwapPair<TestList>::Result >::print();
	}
};
#endif // __FUNC_PROG__
#ifndef __SORT_H__
#define __SORT_H__

#include "fp.h"

//
// Insert sort
//
template<int n, class NList> struct Insert;

template<int n> struct Insert<n, Empty>{
	typedef List<n, Empty> Result;
};

template<int n, class NList> struct Insert{
	template<bool x> struct Branch;
	template<> struct Branch<true>{
		typedef List<n, NList> Result;
	};
	template<> struct Branch<false>{
		typedef List<
			NList::First,
			typename Insert<n, typename NList::Rest>::Result
		> Result;
	};
	typedef typename Branch<(n<NList::First)>::Result Result;
};

template<class NList> struct Sort;

template<> struct Sort<Empty>{
	typedef Empty Result;
};

template<class NList> struct Sort{
	typedef typename Insert<
		NList::First,
		typename Sort<typename NList::Rest>::Result 
	>::Result Result;
};

//
// Binary tree sort
//
template<int n, class NTree> struct InsertToTree;

template<int n> struct InsertToTree<n, Empty>{
	typedef Tree<Empty, n, Empty> Result;
};

template<int n, class NTree> struct InsertToTree{
	template<bool x> struct If;

	template<> struct If<true>{
		typedef Tree<
			typename InsertToTree<n, typename NTree::Left>::Result,
			NTree::value,
			typename NTree::Right> Result;
	};
	template<> struct If<false>{
		typedef Tree<
			typename NTree::Left,
			NTree::value,
			typename InsertToTree<n, typename NTree::Right>::Result
			> Result;
	};

	typedef typename If< (n<NTree::value) >::Result Result;
};

template<class NList> struct BuildSortTree;

template<> struct BuildSortTree<Empty>{
	typedef Empty Result;
};

template<class NList> struct BuildSortTree{
	typedef typename InsertToTree<
		NList::First,
		typename BuildSortTree<typename NList::Rest>::Result
	>::Result Result;
};

//
// Quick sort
//
template<
	template<int, int> class Compare, 
	class NList, 
	int pivot
> struct Filter{
	template<bool x> struct If;
	template<> struct If<true>{
		typedef List<NList::First,
			typename Filter<Compare, typename NList::Rest, pivot>::Result
		> Result;
	};
	template<> struct If<false>{
		typedef typename 
			Filter<Compare, typename NList::Rest, pivot>::Result Result;
	};

	typedef typename
		If< Compare<NList::First, pivot>::value >::Result Result;
};

template<template<int, int> class Compare, int pivot> 
struct Filter<Compare, Empty, pivot>{
	typedef Empty Result;
};

template<int x, int y> struct Less{
	static const bool value=(x<y);
};

template<int x, int y> struct Greater{
	static const bool value=(x>y);
};

template<class NList1, class NList2> struct Join{
	typedef typename Join<
		typename Append<NList1, NList2::First>::Result,
		typename NList2::Rest>::Result Result;
};

template<class NList1> struct Join<NList1, Empty>{
	typedef NList1 Result;
};

template<class NList> struct QuickSort{
	typedef typename Join<
		typename Append<
			typename QuickSort<
				typename Filter<Less, NList, NList::First>::Result 
			>::Result,
			NList::First
		>::Result,
		typename Filter<Greater, NList, NList::First>::Result
	>::Result Result;
};

template<> struct QuickSort<Empty>{
	typedef Empty Result;
};

struct TestSort: public TestCase{
	void test(){
		TestScript<TestSort>::begin(this)
			<<&TestSort::testInsertSort
			<<&TestSort::testTreeSort
			<<&TestSort::testQuickSort
			<<end;
	}

	typedef List<7, List<14, List<9, List<11, List<5, List<17, Empty>>>>>> TestList;

	void testInsertSort(){
		std::cout<<"test insert sort: ";
		Print< Sort<TestList>::Result >::print();
	}

	void testTreeSort(){
		std::cout<<"test BTree sort: ";
		PrintTree< BuildSortTree<TestList>::Result >::print();
		std::cout<<"\n";
	}

	void testQuickSort(){
		std::cout<<"test quick sort: ";
		Print< QuickSort<TestList>::Result >::print();
		std::cout<<"\n";
	}
};

#endif //__SORT_H__
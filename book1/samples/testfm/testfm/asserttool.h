#ifndef __ASSERT_TOOL__
#define __ASSERT_TOOL__

#include <iostream>

inline void assertTrue(bool exp){ 
	if(exp)
		std::cout<<"OK\n";
	else
		std::cout<<"Failed\n";
}

template<class T>
inline void assertEqual(T x, T y){
	if(x == y)
		std::cout<<"OK\n";
	else{
		std::cout<<"Failed. x="<<x<<", y="<<y<<"\n";
	}
}

#endif //#ifndef __ASSERT_TOOL__
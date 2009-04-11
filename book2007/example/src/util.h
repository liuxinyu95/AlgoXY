#ifndef __EVAL_UTIL__
#define __EVAL_UTIL__

#include <iostream>
#include <sstream>

struct util{
	/*refer to: 
	*http://www.codeguru.com/forum/showthread.php?t=231054
	*/
	template <class T>
	static bool str_val(T& val, const std::string& s, 
		std::ios_base& (*fmt)(std::ios_base&)=std::dec)
	{
		std::istringstream iss(s);
		return !(iss >> fmt >> val).fail();
	}

	// refer to:
	// http://www.codeproject.com/string/string_conversions.asp
	//
	template<class T>
	static std::string val_str(const T& val){
		std::stringstream strm;
		strm << val;
		return strm.str();
	}
};
#endif //__EVAL_UTIL

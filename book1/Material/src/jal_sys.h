#ifndef _JCL_SYS_H_
#define _JCL_SYS_H_

#include "jal_common.h"
#include <iostream>

namespace jal{

//
// as singleton	
//
class jal_sys {
public:
	static jal_sys& instance(){
		static jal_sys inst;
		return inst;
	}
	
	const bool is_sys_ok() const { return _sys_ok;	}
	
private:
	jal_sys(){
		//
		// init stuffs...
		//
		_sys_ok = true;
	}

	jal_sys(const jal_sys&);
	jal_sys& operator = (const jal_sys&);
	virtual ~jal_sys(){}

	bool _sys_ok;
};

inline static bool is_sys_ok() {
	return jal_sys::instance().is_sys_ok();
}

}//end namespace jal
#endif //_JAL_SYS_H_

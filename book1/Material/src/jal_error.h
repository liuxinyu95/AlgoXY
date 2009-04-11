#ifndef _JCL_ERROR_H_
#define _JCL_ERROR_H_

#include <string>

namespace jcl {

class jcl_error{
public:
	jcl_error(bool status, std::string msg): _status(status), _msg(msg){}
	jcl_error(std::string msg) : _status(false), _msg(msg){}
	
	jcl_error(const jcl_error& v) {
		_status = v._status;
		_msg	= v._msg;
	}
	
	bool operator == (const jcl_error& v) {
		return (_status == v._status && _msg == v._msg);
	}
	
	bool operator == (const bool& v) {
		return _status == v;
	}
	
	const is_ok() const {
		return _status;
	}
	
	const what() const {
		return _msg;
	}
	
private:
	bool 		_status;
	std::string _msg;
};	

inline static jcl_error& jcl_ok(){
	static jcl_error jcl_ok(true, "OK");
	return jcl_ok;
}

}//end namespace jcl

#endif //_JCL_ERROR_H_

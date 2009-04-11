#ifndef _JAL_LOGIN_H_
#define _JAL_LOGIN_H_

#include "jal_common.h"
#include "jal_sys.h"
#include <stdexcept>

namespace jal{

struct AuthTicket{};

class jal_login {
public:
	jal_login(std::string user = "default" , std::string passwd = "") throw(std::runtime_error)
	{
		init();
		//
		// perform low-level login
		//
	}
	
	virtual ~jal_login() {
		//
		// perform log-out;
		//
	}
	
	const AuthTicket& get_ticket() const {
		return _ticket;
	}
	
private:
	jal_login() {}
	jal_login(const jal_login& v);
	jal_login& operator=(const jal_login& v);
	void init() throw(std::runtime_error);
	
	AuthTicket	_ticket;
};

//
// Note: user must manage the it by herself.
//
inline jal_login* login(std::string user, std::string passwd) {
	return new jal_login(user, passwd);
}
	
inline void logout(jal_login* ticket) {
	delete ticket;
}


}//end namespace jal
#endif //_JAL_LOGIN_H_

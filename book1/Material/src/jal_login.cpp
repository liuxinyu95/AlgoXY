#include "jal_login.h"

using namespace std;

namespace jal{

void jal_login::init() throw(runtime_error)
{
	if(!is_sys_ok())
		throw std::runtime_error("system not OK");
			
	//
	// init SCORE stuffs...
	//
	
}

}//end ns jal
//end

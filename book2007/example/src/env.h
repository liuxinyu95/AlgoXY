#ifndef __PLIS_ENV__
#define __PLIS_ENV__

#include <map>

class expr;

//
//environment
//
class Env{
public:
	Env():outter(0){}
	Env(const expr& vars, const expr& vals):outter(0){append(vars, vals);}
	Env(const Env& ref):var_expr(ref.var_expr), outter(ref.outter){}

	const Env& operator=(const Env& ref){
		if(this == &ref) return *this;
		var_expr=ref.var_expr;
		outter = ref.outter;
		return *this;
	}

	~Env(){
		//need to delete?
	}

	const bool empty();

	expr look_up(const expr& exp);

	Env extend(const expr& vars, const expr& vals);

	void append(const expr& vars, const expr& vals);

	void define(const expr& var, const expr& val);

	static Env setup_global();

	static Env global_env;

	//for debug
	void list();

private:
	std::map<std::string, expr> var_expr;
	Env* outter;	//just reference, not own
};

#endif //__PLIS_ENV__

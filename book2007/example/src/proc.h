#ifndef __PLIS_PROC__
#define __PLIS_PROC__

#include "expr.h"
#include "env.h"

//prim proc
static const char* prim_proc_names[]=
	{"car", "cdr", "cons", "null?", "+", "-", "*", "/", "=", "<"};

class procedure{
public:
	virtual expr apply()=0;
};

class prim_proc: public procedure{
public:
	prim_proc(const expr& _proc, const expr& _args)
		:proc(_proc), args(_args){}

	expr apply(){
		//all has prim(+)'s form
		//using raw c++ code.
		
		if(proc.to_string()=="car")
			return *args.begin();

		if(proc.to_string()=="cdr")
			return expr(++args.begin(), args.end());
		
		//cons

		//logical
		if(proc.to_string()=="null?")
			return args.empty()? expr("true"):expr("false");

		if(proc.to_string()=="="){
			if(args.begin()->to_string()==(++args.begin())->to_string())
				return expr("true");
			else
				return expr("false");
		}

		//<

		//arithmetic
		if(proc.to_string()=="+")
			return binary_op(prim_proc::plus(), args);

		if(proc.to_string()=="-")
			return binary_op(prim_proc::minus(), args);

		if(proc.to_string()=="*")
			return binary_op(prim_proc::multiply(), args);

		if(proc.to_string()=="/")
			return binary_op(prim_proc::divide(), args);

		throw std::string("unknown primitive proc")+proc.to_string();
	}

	static const bool is_prim_proc(const expr& exp){
		for(int i=0; i<sizeof(prim_proc_names)/sizeof(char*); ++i)
			if(exp.to_string() == prim_proc_names[i])
				return true;
		return false;
	}

private:
	template<class Op>
	static expr binary_op(Op op, const expr& args){ //Op& op doesn't work!
		long ires=0;
		double fres=0.0;
		bool is_float=false;

		if(args.get_list().size()<2)
			throw std::string("#args is not enough");

		expr::EList::const_iterator it=args.get_list().begin(); 
		if(!util::str_val<long>(ires, it->get_symbol()))
			if(util::str_val<double>(fres, it->get_symbol()))
				is_float=true;
			else
				throw std::string("unknown value: ")+it->get_symbol();

		for(++it; it!=args.get_list().end(); ++it)
		{
			long ix;
			double fx;
			if(util::str_val<long>(ix, it->get_symbol())){
				ires=op(ires, ix);
			}
			else if(util::str_val<double>(fx, it->get_symbol())){
				is_float=true;
				fres=op(fres,fx);
			}
			else
				throw std::string("unknown value: ")+it->get_symbol();
		}

		if(is_float){
			fres=op(fres, static_cast<double>(ires));
			return expr(util::val_str(fres));
		}
		else
			return expr(util::val_str(ires));
	}

	//op functors
	struct plus{
		template<class T> T operator()(T x, T y){ return x+y; }
	};

	struct minus{
		template<class T> T operator()(T x, T y){ return x-y; }
	};

	struct multiply{
		template<class T> T operator()(T x, T y){ return x*y; }
	};

	struct divide{
		template<class T> T operator()(T x, T y){ return x/y; } //what if y==0?
	};

	expr proc;
	expr args;
};

class user_proc/*: public procedure*/{
public:
	typedef expr::EList EList;
	typedef expr::ExprIt ExprIt;

	//expr apply(){
	//	return expr();
	//}

	static expr make(const expr& exp, Env& env){
		return expr("proc").append(lambda_params(exp)).append(lambda_body(exp)).set_env(env);
	}

	static expr make_lambda(EList params, ExprIt body, ExprIt end){
		//form: lambda( (x, y), +(x, y) )
		expr res=expr("lambda").append(params);
		for(ExprIt it=body; it!=end; ++it)
			res.append(*it);
		return res;
	}

	static expr lambda_params(const expr& exp){	return *exp.begin(); }

	static expr lambda_body(const expr& exp){ return expr(++exp.begin(), exp.end()); }
};

#endif //__PLIS_PROC__

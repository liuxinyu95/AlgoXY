#ifndef __EVALUATOR__
#define __EVALUATOR__

#include <map>
#include <list>
#include <string>
#include <algorithm>
#include <iostream>

#include "env.h"
#include "expr.h"
#include "proc.h"

class definition{
public:
    definition(const expr& _exp):exp(_exp){}
	definition(const expr& _exp, Env& env):exp(_exp){exp.set_env(env);}
	virtual ~definition(){}
	virtual expr var()=0;
	virtual expr val()=0;
protected:
	expr exp;
};

//
// has form of: define(var, value)
//
class var_def: public definition{
public:
    var_def(const expr& _exp):definition(_exp){}
	expr var(){	return *exp.begin();}
	expr val(){	return *(++exp.begin());}
};

//
// has form of: define(func(x, y), +(x, y,...))
//
class proc_def: public definition{
public:
    proc_def(const expr& _exp, Env& env):definition(_exp){}
	expr var(){	return expr(exp.begin()->get_symbol());}
	expr val(){
		return user_proc::make_lambda(
				  exp.begin()->get_list(), 
				  ++exp.begin(), exp.end());
	}
};

struct def_factory{
	static definition* create(const expr& exp, Env& env){
		if(exp.begin()->is_symbol())
			return new var_def(exp);
		return new proc_def(exp, env);
	}
};

class Evaluator{
public:
	static Evaluator& instance(){
		static Evaluator inst;
		return inst;
	}

	expr eval(const expr& exp){
		return eval(exp, Env::global_env);
	}

	expr eval(const expr& exp, Env& env){
		if(exp.is_self_eval())
			return exp;

		else if(exp.is_var())
			return env.look_up(exp);

		else if(exp.is_def())
			return eval_def(exp, env);

		else if(exp.is_if())
			return eval_if(exp, env);

		else if(exp.is_lambda())
			return user_proc::make(exp, env);

		else if(exp.is_app())
			return apply(
				eval(get_operator(exp), env),
				value_list(get_operands(exp), env));

		else
			return expr(std::string("error. unknown expression\n")+exp.to_string());
	}

protected:
	expr apply(const expr& proc, const expr& args){
		if(prim_proc::is_prim_proc(proc)){
			return prim_proc(proc, args).apply();
		}
		else{
			expr::ExprIt body=++proc.get_list().begin();
			expr::ExprIt params=proc.get_list().begin();
			return eval_seq(*body, proc.get_env().extend(*params, args));
		}
	}

	Evaluator(){}

private:
	expr value_list(const expr& exp, Env& env){
		expr vals;
		for(expr::EList::const_iterator it=exp.get_list().begin(); 
			it!=exp.get_list().end(); ++it)
				vals.append(eval(*it, env));
		return vals;
	}

	expr eval_def(const expr& exp, Env& env){
		definition* def=def_factory::create(exp, env);
		env.define(def->var(), eval(def->val(), env));
		delete def;
		return expr("OK");
	}

	expr eval_seq(const expr& exp, Env env){
		expr res;	//the return value of sequence is the result of the last sentence.
		for(expr::ExprIt it=exp.get_list().begin(); it!=exp.get_list().end(); ++it)
			res=eval(*it, env);
		return res.set_env(env);	//if there are local defines
	}

	expr eval_if(const expr& exp, Env& env){
		//if( predicate, consequent, alternative )
		if(eval(*exp.get_list().begin(), env).get_symbol()=="true")
			return eval(*(++exp.get_list().begin()), env);
		return eval(*(++(++exp.get_list().begin())), env);
	}

	expr get_operator(const expr& exp){ return expr(exp.get_symbol()); }

	expr get_operands(const expr& exp){ return expr(exp.get_list()); }
};

#endif //__EVALUATOR__

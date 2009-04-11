#include "env.h"
#include "expr.h"
#include "proc.h"

const bool Env::empty(){
	if(!var_expr.empty())
		return false;
	if(outter && !outter->empty())
		return false;
	return true;
}

expr Env::look_up(const expr& exp){
	if(prim_proc::is_prim_proc(exp))
		return exp;

	//search var recursively.
	if(var_expr.find(exp.get_symbol())!=var_expr.end())
		return var_expr[exp.get_symbol()];
	else if(outter)
		return outter->look_up(exp);
	else
		throw std::string("Unbound var: ")+exp.to_string();
}

Env Env::extend(const expr& vars, const expr& vals){
	//local_env-->outter_env-->...-->global_env
	if(vars.get_list().size() == vals.get_list().size()){
		Env res(vars, vals);
		res.outter=this;
		return res;
	}
	else
		throw std::string("#vars != #vals: ")+vars.to_string()+vals.to_string();
}

void Env::append(const expr& vars, const expr& vals){
	expr::EList::const_iterator it_var, it_val;
	for(it_var=vars.get_list().begin(), it_val=vals.get_list().begin(); 
		it_var!=vars.get_list().end(); 
		++it_var, ++it_val)
	{
		var_expr[it_var->to_string()]=(*it_val);
	}
}

void Env::define(const expr& var, const expr& val){
	if(var.empty())
		throw std::string("empty var name\n");
	var_expr[var.get_symbol()]=val;
}

//just for debug
void Env::list(){
	for(std::map<std::string, expr>::iterator it=var_expr.begin(); it!=var_expr.end(); ++it)
		std::cout<<it->first<<"=\t"<<it->second.to_string()<<"\n";
}

//static 
Env Env::setup_global(){
	Env global_env;
	expr vars, vals;
	vars.append("true").append("1");
	vals.append("true").append("0");
	global_env.append(vars, vals);
	return global_env;
}

//static member init
Env Env::global_env=Env::setup_global();

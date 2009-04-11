#ifndef __PLIS_EXPR__
#define __PLIS_EXPR__

#include <list>
#include "util.h"
#include "env.h"

//
// syntax of 'plis'
// expr:= word £ü word list | list
//	 word:= [a..z0..9-><+-*/=]
//	 list:= (expr, expr, ... expr)
//

//
// data structure for syntax in C++
//

class expr{
public:
	typedef std::list<expr> EList;
	typedef EList::const_iterator ExprIt;

	//ctors
	expr(){}
	expr(const std::string& sym):symbol(sym), env(0){}
	expr(const char* str):symbol(str), env(0){}
	expr(const EList& loe):elist(loe), env(0){}
	expr(const std::string& sym, const EList& loe):symbol(sym),elist(loe), env(0){}
	expr(const char* str, const EList& loe):symbol(str),elist(loe),env(0){}
	expr(ExprIt from, ExprIt to):elist(from, to), env(0){}

	//copy ctor and assingment
	expr(const expr& ref):symbol(ref.symbol), elist(ref.elist), env(ref.env){};
	const expr& operator=(const expr& ref){
		symbol=ref.symbol;
		elist =ref.elist;
		env=ref.env;
		return *this;
	}

	const bool operator==(const expr& ref) const{
		return symbol==ref.symbol && elist==ref.elist;
	}

	~expr(){}

	// type test
	const bool empty() const { 
		return symbol.empty() && elist.empty();	
	}
	const bool is_list() const { 
		return symbol.empty() && (!elist.empty());
	}
	const bool is_symbol() const {
		return elist.empty() && (!symbol.empty());
	}

	const bool is_self_eval() const{
		//TODO: is number? or literal string?
		if(is_symbol()){
			long ix;
			if(util::str_val<long>(ix, symbol))
				return true;
			double fx;
			if(util::str_val<double>(fx, symbol))
				return true;
		}
		return false;
	}

	const bool is_var() const{
		return is_symbol();	//weak than is_self_eval. must after it.
	}

	const bool is_app() const{
		return (!symbol.empty()) && (!elist.empty());
	}

	const bool is_def() const{ return symbol=="define"; }

	const bool is_lambda() const { return symbol=="lambda"; }

	const bool is_if() const { return symbol=="if"; }

	//for debug
	std::string to_string() const{
		std::string res;
		if(!elist.empty()){
			res+="(";
			for(EList::const_iterator it=elist.begin(); it!=elist.end(); ++it)
				res+=it->to_string()+",";
			res[res.length()-1]=')';
		}
		return symbol+res;
	}

	//access
	std::string get_symbol() const{ return symbol; }
	const EList& get_list() const { return elist; }
	Env& get_env() const { return *env; }
	
	const bool empty_env() const { return env==0; }

	// return the first element in the elist
	ExprIt begin() const{ return elist.begin(); }
	ExprIt end() const{ return elist.end(); }

	expr& append(const expr& x){ elist.push_back(x); return *this; }

	expr& set_env(Env& ref){ 
		if(&ref == &Env::global_env)
			env=&ref; 
		else
			env=new Env(ref);
		return *this; 
	}

private:
	std::string symbol;
	std::list<expr> elist; //list of expr
	Env* env;
};

#endif //__PLIS_EXPR__

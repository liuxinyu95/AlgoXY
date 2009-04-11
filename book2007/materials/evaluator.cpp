#include <map>
#include <list>
#include <string>
#include <iostream>

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

	//ctors
	expr(){}
	expr(const std::string& sym):symbol(sym){}
	expr(const char* str):symbol(str){}
	expr(const EList& loe):elist(loe){}
	expr(const std::string& sym, const EList& loe):symbol(sym),elist(loe){}
	expr(const char* str, const EList& loe):symbol(str),elist(loe){}

	//copy ctor and assingment
	expr(const expr& ref):symbol(ref.symbol), elist(ref.elist){};
	const expr& operator=(const expr& ref){
		symbol=ref.symbol;
		elist =ref.elist;
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

	//for debug
	std::string to_string(){
		std::string res;
		if(!elist.empty()){
			res+="(";
			for(EList::iterator it=elist.begin(); it!=elist.end(); ++it)
				res+=it->to_string()+",";
			res[res.length()-1]=')';
		}
		return symbol+res;
	}

	expr car(){
		if(!elist.empty())
			return *elist.begin();
		throw std::string("invoke car on empty\n");
	}

	expr cdr(){
		if(elist.empty())
			throw std::string("invoke cdr on empty\n");

		EList::iterator it=elist.begin();
		if(++it!=elist.end())
			return expr(EList(it, elist.end()));
		else
			return expr(); // null
	}

	expr& append(const expr& x){ elist.push_back(x); return *this; }

private:
	std::string symbol;
	std::list<expr> elist; //list of expr
};

//environment
/*
struct Env{
	Env():outter(0){}
	std::map<std::string, Exp> VarVal;
	Env* outter;
};
*/


//value eval(exp env)
/*
std::string eval(Exp* exp, Env env){
	if(isApp(exp)){
		applay(
			eval(get_operator(exp), env),
			value_list(get_operands(exp), env));
	}
	else{
		return std::string("error. unknown expression\n")+exp.toString();
	}
}
*/

void test_expr(){
	//1. +(1, 2)
	expr e1=expr("+").append(expr("1")).append(expr("2"));
	std::cout<<"e1:"<<e1.to_string()<<"\n";

	//2. +(1, *(3, 4))
	expr e2=expr("+").append(expr("1")).append(
			 expr("*").append("3").append("4"));
	std::cout<<"e2:"<<e2.to_string()<<"\n";

	//3.
	try{
		std::cout<<"car(e2):"<<e2.car().to_string()<<"\n"
				 <<"cadr(e2):"<<e2.cdr().car().to_string()<<"\n";
	}
	catch(std::string &e){
		std::cout<<"exception: "<<e;
	}
}

int main(int argc, char* argv[]){
	test_expr(); //expr test
}


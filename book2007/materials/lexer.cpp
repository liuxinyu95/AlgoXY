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

// forward decarations
class Expr;
class Word;
class List;

class Word{
public:
	Word(){}
	Word(const std::string& x):symbol(x){}
	Word(const Word& x):symbol(x.symbol){}
	Word& operator=(const Word& x){
		symbol=x.symbol;
		return *this;
	}

	const bool operator==(const Word& x){ return symbol == x.symbol; }

	const bool empty(){ return symbol.empty(); }

	std::string toString(){ return symbol; }

private:
	std::string symbol;
};

class List{
	typedef std::list<Expr> ExprList;
public:
	List(){}
	List(const List& x):exprs(x.exprs){}
	List(const ExprList& x):exprs(x){}
	List& operator=(const List& x){
		exprs=x.exprs;
		return *this;
	}

	const bool operator==(const List& x){ return exprs==x.exprs; }

	const bool empty(){ return exprs.empty(); }

	const std::string toString();

	Expr car();
	List cdr();
	List& append(Expr& x);

private:
	ExprList exprs;
};

class Expr{
public:
	Expr(const Word& _proc);
	Expr(const List& _list);
	Expr(const Word& _word, const List& _list);
	Expr(const Expr& x);
	Expr& operator=(const Expr& x);

	const bool operator==(const Expr& x);

	~Expr(){}

	const bool empty();
	const bool isList();
	const bool isSymbol();
	std::string toString();

private:
	Word word;
	List list;
};

Expr::Expr(const Word& _word):word(_word){}
Expr::Expr(const List& _list): list(_list){}
Expr::Expr(const Word& _word, const List& _list): word(_word), list(_list){}
Expr::Expr(const Expr& x):word(x.word),list(x.list){}
Expr& Expr::operator=(const Expr& x){
	word=x.word;
	list=x.list;
	return *this;
}

const bool Expr::operator==(const Expr& x){
	return word==x.word && list ==x.list;
}

const bool Expr::empty(){ return word.empty() && list.empty(); }

const bool Expr::isList(){
	return word.empty() && (!list.empty());
}

const bool Expr::isSymbol(){
	return list.empty() && (!word.empty());
}

std::string Expr::toString(){
	return word.toString()+list.toString();
}

const std::string List::toString(){
	std::string res;
	if(!empty()){
		res+="(";
		for(ExprList::iterator it=exprs.begin(); it!=exprs.end(); ++it)
			res+=it->toString()+",";
		res[res.length()-1]=')';
	}
	return res;
}

Expr List::car(){
	if(!exprs.empty())
		return *exprs.begin();
	throw std::string("invoke car on empty\n");
}

List List::cdr(){
	if(exprs.empty())
		throw std::string("invoke cdr on empty\n");

	ExprList::iterator it=exprs.begin();
	if(++it!=exprs.end()){
		ExprList sublist(++it, exprs.end());
		return List(sublist);
	}
	else
		return List();
}

List& List::append(Expr& x){ exprs.push_back(x); return *this; }

//
//lexer
//

const char delimiter[]={'(', ')', ','};
const char whitespace[]={' ', '\t', '\n'};

const bool is_delimiter(char c){
	for(int i=0; i<sizeof(delimiter)/sizeof(char); ++i)
		if(c==delimiter[i])
			return true;
	return false;
}

const bool is_white(char c){
	for(int i=0; i<sizeof(whitespace)/sizeof(char); ++i)
		if(c==whitespace[i])
			return true;
	return false;
}

class Lexer{
	typedef std::string::iterator StrIt;

	Lexer(std::string str):input(str){}
	
	Lexer& proc(){
		try{
			for(StrIt it=input.begin(); it!=input.end(); )
				program.push_back(lex_exp(it));
		}
		catch(std::string& err){
			std::cout<<"error"<<err;
		}
		return *this;
	};

	std::string toString(){
		std::string res;
		for(std::list<Expr>::iterator it=program.begin(); it!=program.end(); ++it)
			res+=it->toString()+"\n";
		return res;
	}

	Expr lex_exp(StrIt& it){
		return (lex_word(it), lex_list(it));
	}

	Word lex_word(StrIt& it){
		std::string symbol;
		for(;it!=input.end(); ++it){

			// starter proc
			if(is_white(*it) && symbol.empty())
				continue;

			// terminator proc
			if(is_delimiter(*it) || is_white(*it))
				break;

			symbol.push_back(*it);
		}
		return symbol;
	}

	StrIt trim_to(StrIt& it, char c){
		for(;it!=input.end();++it){
			if(*it==c){
				return ++it;
			}
			else if(!is_white(*it))
				throw std::string("invalide char before list\n");
		}
		return it;
	}

	List lex_list(StrIt& it){
		List res;

		//valid starter?
		if(it!=input.end() && !is_white(*it) && *it!=',' && *it!='(')
			throw std::string("invalid char before list");

		for(it=trim_to(it, '('); it!=input.end(); ++it){
			res.append(lex_exp(it));
			
			//delimiter proc
			if(*it==',')
				continue;

			//terminator proc
			if(*it == ')'){
				++it;
				return res;
			}
		}
		if(!res.empty())
			throw std::string("unpaired (\n");
		return res;
	}

private:
	std::string input;
	std::list<Expr> program;
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

int main(int argc, char* argv[]){
	std::cout<<"test lexer\n";

	std::string case1="+(1, 2)";
	std::cout<<"lex "<<case1<<"==>"
			 <<Lexer(case1).proc().toString()<<"\n";

	/*
	std::string case2="+(1, *(2, 3))";
	std::cout<<"lex "<<case2<<"==>"
		<<Lexer::proc(case2)->toString()<<"\n";

	std::string case3="+(1, *(2, 3), )";
	std::cout<<"lex "<<case3<<"==>"
		<<Lexer::proc(case3)->toString()<<"\n";
	*/
}


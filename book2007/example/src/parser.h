#ifndef __PARSER__
#define __PARSER__

#include <list>
#include "lexer.h"
#include "expr.h"

//
// syntax of 'plis'
// expr:= word £ü word list | list
//	 word:= [a..z0..9-><+-*/=]
//	 list:= (expr, expr, ... expr)
//

struct parser{
	expr parse(std::list<token> _tokens){
		tokens=_tokens;
		return expression();
	}

	expr expression(){
		expr res;
		//expect word
		if(expect(token::word))
			res=word();

		//expect "(" or null
		if(expect(token::left_bracket)){
			drop();	//drop "(";
			bool hasNext=true;
			while(hasNext){
				res.append(expression());

				if(expect(token::right_bracket))
					hasNext=false;

				if(expect(token::right_bracket) || expect(token::deliminator))
					drop(); //drop "," or ")"
				else
					throw std::string("syntax error, unpaired (");
			}
		}
		
		return res;
	}

	bool expect(int type){
		if(!tokens.empty())
			return tokens.front().type == type;
		return false;
	}

	void drop(){
		tokens.pop_front();
	}

	expr word(){
		expr res(tokens.front().content);
		drop();
		return res;
	}

private:
	std::list<token> tokens;
};

#endif //__PARSER__
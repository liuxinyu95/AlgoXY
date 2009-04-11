#ifndef __LEXER__
#define __LEXER__

#include <list>
#include <string>

//token definition
struct token{
	enum{word, left_bracket, right_bracket, deliminator};

	token(int _type, std::string _content):type(_type), content(_content){}
	token(int _type):type(_type){}
	token(const token& x):type(x.type), content(x.content){}
	const token& operator=(const token& x){
		if(&x != this){
			type=x.type;
			content=x.content;
		}
		return *this;
	}

	const bool operator==(const token& x) const{
		return type==x.type && content==x.content;
	}

	int type;
	std::string content;
};

struct lexer{
	//
	//using a finit machine in lexing
	//	with maximal match rule
	//
	static std::list<token> lex(std::string input){
		std::list<token> res;
		for(std::string::iterator it=input.begin(); it!=input.end(); /*++it*/){
			while(it!=input.end() && is_whitespace(*it)){
				++it;//skip white space
			}

			std::string lexeme;
			while(it!=input.end() && is_word(*it)){
				lexeme.push_back(*it++);
			}

			if(lexeme!="")
				res.push_back(token(token::word, lexeme));

			if(it!=input.end() && is_deliminator(*it)){
				res.push_back(token(token::deliminator));
				++it;
			}

			if(it!=input.end() && *it == '('){
				res.push_back(token(token::left_bracket));
				++it;
			}

			if(it!=input.end() && *it == ')'){
				res.push_back(token(token::right_bracket));
				++it;
			}
		}
		return res;
	}

	void static print_tokens(std::list<token> coll){
		for(std::list<token>::iterator it=coll.begin(); it!=coll.end(); ++it){
			switch(it->type){
				case token::word:
					std::cout<<it->content;
					break;
				case token::left_bracket:
					std::cout<<'(';
					break;
				case token::right_bracket:
					std::cout<<')';
					break;
				case token::deliminator:
					std::cout<<',';
					break;
			}
		}
		std::cout<<'\n';
	}

	static bool is_word(char c){
		return is_char(c) && !is_whitespace(c) && !is_deliminator(c);
	}

	static bool is_char(char c){
		return ('a'<=c && c<='z') || ('A'<=c && c<='Z') || ('0'<=c && c<='9') || is_op(c);
			
	}

	static bool is_op(char c){
		return c=='.' || 
			c == '+' || c == '-' || c == '*' || c == '/' ||
			c == '=' || c == '<' || c == '>' || c == '?' ;
	}

	static bool is_whitespace(char c){
		return c=='\n' || c=='\t' || c== '\r' || c==' ';
	}

	static bool is_deliminator(char c){
		return c == ',';
	}
};

#endif

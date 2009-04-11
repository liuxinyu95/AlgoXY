#include "test/testcase.h"
#include "test/testscript.h"
#include "test/testcaller.h"
#include "test/asserttool.h"
#include "evaluator.h"
#include "lexer.h"
#include "parser.h"

class TestEvaluator : public TestCase
{
	void test(){
		TestScript<TestEvaluator>::begin(this)
			<<&TestEvaluator::testLexer
			<<&TestEvaluator::testParser
			<<&TestEvaluator::testExpr
			<<&TestEvaluator::testEvalBasic
			<<&TestEvaluator::testInteprete
			<<&TestEvaluator::testProgram
			<<&TestEvaluator::testDriverLoop
			<<end;
	}

	void testLexer(){
		std::cout<<"test lexer\n";
		lexer::print_tokens(lexer::lex("1"));
		lexer::print_tokens(lexer::lex("+(1, 2)"));
		lexer::print_tokens(lexer::lex("+(1, *(3, 4))"));
		lexer::print_tokens(lexer::lex("define(one, 1)"));
		lexer::print_tokens(lexer::lex("one"));
		lexer::print_tokens(lexer::lex("define(two, +(one, 1))"));
		lexer::print_tokens(lexer::lex("define(plus(x,y), +(x, y))"));
		lexer::print_tokens(lexer::lex("plus(1, 2)"));
		lexer::print_tokens(lexer::lex("define(frac(n), if(=(n, 0), 1, *(n, frac(-(n, 1)))))"));
		lexer::print_tokens(lexer::lex("frac(6)"));
	}

	void testParser(){
		std::cout<<"test parser\n";
		parser p;
		std::cout<<p.parse(lexer::lex("1")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("+(1, 2)")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("+(1, *(3, 4))")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("define(one, 1)")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("one")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("define(two, +(one, 1))")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("define(plus(x,y), +(x, y))")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("plus(1, 2)")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("define(frac(n), if(=(n, 0), 1, *(n, frac(-(n, 1)))))")).to_string()<<'\n';
		std::cout<<p.parse(lexer::lex("frac(6)")).to_string()<<'\n';
	}
	
	void testExpr(){
		std::cout<<"test expr\n";

		try{
			//1. +(1, 2)
			expr e1=expr("+").append("1").append("2");
			std::cout<<"e1:"<<e1.to_string()<<"\n";

			//2. +(1, *(3, 4))
			expr e2=expr("+").append("1").append(
				expr("*").append("3").append("4"));
			std::cout<<"e2: "<<e2.to_string()<<"\n";

			//3. car(e2), cadr(e2)
			std::cout<<"car(e2):"<<e2.begin()->to_string()<<"\n"
				<<"cadr(e2): "<<(++e2.begin())->to_string()<<"\n";
		}
		catch(std::string &e){
			std::cout<<"exception: "<<e;
		}
	}

	void testEvalBasic(){
		try{
			std::cout<<"test eval basic\n";

			//1. +(1, 2)
			expr e1=expr("+").append("1").append("2");
			std::cout<<"eval: "<<e1.to_string()<<"=";
			std::cout<<Evaluator::instance().eval(e1).to_string()<<"\n";

			//2. +(1, *(2, 3))
			expr e2=expr("+").append("1").append(
				expr("*").append("2").append("3"));
			std::cout<<"eval: "<<e2.to_string()<<"=";
			std::cout<<Evaluator::instance().eval(e2).to_string()<<"\n";

			//3. 1
			expr e3=expr("1");
			std::cout<<"eval: "<<e3.to_string()<<"=";
			std::cout<<Evaluator::instance().eval(e3).to_string()<<"\n";

			//4. (1, 2)
			/*
			expr e4=expr().append("1").append("2");
			std::cout<<"eval:"<<e4.to_string()<<"=";
			std::cout<<Evaluator::instance().eval(e4).to_string()<<"\n";
			*/

			//5. define(one, 1)
			expr e5=expr("define").append("one").append("1");
			std::cout<<"eval: "<<e5.to_string()<<"=";
			std::cout<<Evaluator::instance().eval(e5).to_string()<<"\n";
			expr e51=expr("one");
			std::cout<<"eval: "<<e51.to_string()<<"="
					 <<Evaluator::instance().eval(e51).to_string()<<"\n";

			//6. define(two, +(one, 1))
			expr e6=expr("define").append("two")
				.append(expr("+").append("one").append("1"));
			std::cout<<"eval: "<<e6.to_string()<<"="
					 <<Evaluator::instance().eval(e6).to_string()<<"\n";
			expr e61=expr("two");
			std::cout<<"eval: "<<e61.to_string()<<"="
					 <<Evaluator::instance().eval(e61).to_string()<<"\n";

			//7. define(plus(x,y), +(x, y))
			expr e7=expr("define").append(expr("plus").append("x").append("y"))
				                  .append(expr("+").append("x").append("y"));
			std::cout<<"eval: "<<e7.to_string()<<"="
					 <<Evaluator::instance().eval(e7).to_string()<<"\n";
			expr e71=expr("plus").append("1").append("2");
			std::cout<<"eval: "<<e71.to_string()<<"="
					 <<Evaluator::instance().eval(e71).to_string()<<"\n";

			//8. define(frac(n), if(=(n, 0), 1, *(n, frac(-(n, 1)))))
			expr e8=expr("define").append(expr("frac").append("n"))
				.append(
					expr("if").append(expr("=").append("n").append("0"))
						.append("1")
						.append(expr("*").append("n").append(
							expr("frac").append(expr("-").append("n").append("1")))));
			std::cout<<"eval: "<<e8.to_string()<<"="
				<<Evaluator::instance().eval(e8).to_string()<<"\n";
			expr e81=expr("frac").append("6");
			std::cout<<"eval: "<<e81.to_string()<<"="
				<<Evaluator::instance().eval(e81).to_string()<<"\n";
		}
		catch(std::string &e){
			std::cout<<"exception: "<<e;
		}
	}

	void testInteprete(){
		std::cout<<"test interprete\n";
		parser p;
		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("1"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("+(1, 2)"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("+(1, *(3, 4))"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("define(one, 1)"))).to_string()<<'\n';
		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("one"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("define(two, +(one, 1))"))).to_string()<<'\n';
		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("two"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("define(plus(x,y), +(x, y))"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("plus(1, 2)"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("define(frac(n), if(=(n, 0), 1, *(n, frac(-(n, 1)))))"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("frac(6)"))).to_string()<<'\n';
	}

	void testProgram(){
		std::cout<<"test programming...\n";
		parser p;
		
		const char* prog1="define(construct(x,y),"
							"lambda((i), "
								"if(=(i,1), "
									"x, "
									"y))) " ;
		std::cout<<prog1<<"\n";
		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex(prog1))).to_string()<<'\n';

		const char* prog2="define(my-cons(x,y),"
							"define(get(i), "
								"if(=(i,1), "
									"x, "
									"y)), " 
							"get)";
		std::cout<<prog2<<"\n";
		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex(prog2))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("define(first(x), x(1))"))).to_string()<<'\n';

		std::cout<<Evaluator::instance().eval(
			p.parse(lexer::lex("define(next(x), x(2))"))).to_string()<<'\n';
	}

	void testDriverLoop(){
		std::cout<<"driver-loop\n";
		parser p;
		std::string e;
		while(true){
			std::cout<<"input an expression:\n";
			if(std::getline(std::cin, e)){
				if(e=="quit")
					break;

				try{
					std::cout<<Evaluator::instance().eval(p.parse(lexer::lex(e))).to_string()<<'\n';
				}
				catch(std::string &err){
					std::cout<<"Error: "<<err<<'\n';
				}
			}
		}
	}
};

namespace{
	//const bool res=TestSuite::instance().add(new TestEvaluator);
}


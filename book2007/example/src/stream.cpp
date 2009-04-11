#include "test/testcase.h"
#include "test/testscript.h"
#include "test/testcaller.h"
#include "test/asserttool.h"
#include "stream.h"


typedef stream_record<int> IntStream;
IntStream* ones=create_series(1, AddTo<int>(0));

//IntStream* integers(){
//	return (new IntStream(1))->setNext(add_series<IntStream>, ones, integers());
//}

class TestStream : public TestCase
{
	void test(){
		TestScript<TestStream>::begin(this)
			<<&TestStream::testExample
			<<&TestStream::testDelay
			<<&TestStream::testInfinity
			<<end;
	}

	void testExample(){
		std::cout<<"test example\n";
		record* r1=new record(90, new record(250, new record(1000, 0)));
		print_rec(r1);
		record* r2=transform(r1, discount);
		print_rec(r2);
		record* r3=filter(r2, Greator<double>(500.0));
		print_rec(r3);
		delete r3;
		delete r2;
		delete r1;
	}

	void testDelay(){
		std::cout<<"test delay eval\n";
		typedef stream_record<double> stream_record;
		stream_record* r1=new stream_record(90, new stream_record(250, new stream_record(1000, 0)));
		print_rec(r1);
		stream_record* r2=transform(r1, discount);
		std::cout<<"transform over\n";
		stream_record* r3=filter(r2, Greator<double>(500.0));
		std::cout<<"1st: "<<getAt(r2, 0)<<"\n";
		std::cout<<"1st got\n";
		std::cout<<"2nd: "<<getAt(r2, 1)<<"\n";
		std::cout<<"2nd got\n";
		std::cout<<"3rd: "<<getAt(r2, 2)<<"\n";
		print_rec(r3);
		delete r3;
		delete r2;
		delete r1;
	}

	void testInfinity(){
		std::cout<<"test infinity\n";
		stream_record<int>* N=create_series(1, AddTo<int>(1));
		std::cout<<"natural number created.\n";
		std::cout<<"7th: "<<getAt(N, 6)<<"\n";
		
		stream_record<int>* N1=add_series(ones, N);
		std::cout<<"7th: "<<getAt(N1, 6)<<"\n";

		IntStream* integers=new IntStream(1);
		integers->setNext(add_series(ones, integers));

		std::cout<<"5th: "<<getAt(integers, 4)<<"\n";
		delete N1;
		delete N;
		delete integers;
	}
};

namespace{
	const bool res=TestSuite::instance().add(new TestStream);
}


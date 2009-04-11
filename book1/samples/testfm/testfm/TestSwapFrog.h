#ifndef __TEST_SWAP_FROG__
#define __TEST_SWAP_FROG__

#include <vector>
#include <list>
#include <algorithm>
#include <utility>
#include "testcase.h"
#include "testscript.h"
#include "testcaller.h"

typedef std::vector<int> FrogLine;

class MoveFrog{
public:
	MoveFrog(const FrogLine& endLine):target(endLine){}

	bool run(FrogLine line){
		steps.push_back(line);
		if(isOK(line)){
			for(std::list<FrogLine>::iterator it=steps.begin(); it!=steps.end(); ++it)
				print(*it);
			return true;
		}
		else{
			std::list<FrogLine> nextLines=moveIt(line);
			if(!nextLines.empty()){
				for(std::list<FrogLine>::iterator it=nextLines.begin(); it!=nextLines.end(); ++it){
					if(run(*it))
						return true;	//find
				}
			}

			//back-track
			if(!steps.empty())
				steps.pop_back();

			return false;
		}
	}

private:
	bool isOK(FrogLine line){ return line==target; }

	std::list<FrogLine> moveIt(FrogLine& line){
		//generate a group of unique (NO duplicated) new lines
		std::list<FrogLine> res;
		unsigned int pos;
		for(pos=0; pos<line.size(); ++pos){
			if(line[pos]==0) break;
		}

		
		if(pos>0){
			//step left --> right
			if(line[pos-1]>0 )
				addNewLine(res, line, pos-1, pos);

			//jump left --> right
			if(pos>=2 && line[pos-1]!=0 && line[pos-2]>0)
				addNewLine(res, line, pos-2, pos);
		}

		if(pos<line.size()-1){
			//step left <-- right
			if(line[pos+1]<0)
				addNewLine(res, line, pos, pos+1);

			//jump left <-- right
			if(pos+2<line.size() && line[pos+1]!=0 && line[pos+2]<0)
					addNewLine(res, line, pos, pos+2);
		}

		return res;
	}

	void addNewLine(std::list<FrogLine>& res, FrogLine line, int pos1, int pos2){
		std::swap(line[pos1], line[pos2]);
		if(std::find(steps.begin(), steps.end(), line)==steps.end())
			res.push_back(line);
	}

	void print(FrogLine& line){
		for(FrogLine::iterator it=line.begin(); it!=line.end(); ++it)
			std::cout<<*it<<", ";
		std::cout<<"\n";
	}

	std::list<FrogLine> steps;
	FrogLine target;
};

class TestFrogLine : public TestCase
{
	void test(){
		TestScript<TestFrogLine>::begin(this)
			<<&TestFrogLine::testRun
			<<end;
	}

	void testRun(){
		const int line0[]={1, 1, 1, 0, -1, -1, -1};
		const int line1[]={-1, -1, -1, 0, 1, 1, 1};
		FrogLine before(line0, line0+sizeof(line0)/sizeof(int));
		FrogLine after (line1, line1+sizeof(line1)/sizeof(int));

		MoveFrog runner(after);
		runner.run(before);
	}
};

#endif
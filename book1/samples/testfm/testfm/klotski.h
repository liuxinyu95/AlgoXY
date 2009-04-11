#ifndef __KLOTSKI__
#define __KLOTSKI__

#include <iostream>
#include <map>
#include <list>
#include <deque>
#include <algorithm>
#include <cstdlib>
#include "testcase.h"
#include "testscript.h"
#include "testcaller.h"
#include "asserttool.h"

struct Point{
	Point():x(0), y(0){}
	Point(int _x, int _y): x(_x), y(_y){}
	Point(const Point& ref):x(ref.x), y(ref.y){}
	virtual ~Point(){}

	const Point& operator=(const Point& ref){
		x = ref.x;
		y = ref.y;
		return *this;
	};

	virtual const bool operator ==(const Point& ref) const{
		return (x == ref.x) && (y == ref.y);
	}

	const Point operator+ (const Point& ref) const{
		return Point(x+ref.x, y+ref.y);
	}

	const Point& operator+= (const Point& ref){
		x+=ref.x;
		y+=ref.y;
		return *this;
	}

	int x;
	int y;
};

struct Size: public Point{
	Size():Point(){}
	Size(int w, int h):Point(w, h){}
	const int width()  const { return Point::x; };
	const int height() const { return Point::y; };
	bool operator==(const Size& ref) const { return width()==ref.width() && height()==ref.height(); }
	bool operator!=(const Size& ref) const { return !(*this == ref); }
};

class Block{
public:
	Block(){}
	Block(const Point& p, const Size& sz, char v)
		:_topLeft(p), _size(sz), _value(v){};

	const Block& operator = (const Block& ref){
		if( *this == ref ) return *this;
		_topLeft = ref._topLeft;
		_size = ref._size;
		_value = ref._value;
		return *this;
	}

	const bool operator == (const Block& ref) const {
		return (_topLeft == ref._topLeft) && (_size == ref._size) && (_value == ref._value);
	}

	Block at(const Point& position){
		return Block(position, _size, _value);
	}

	const Size& size() const { return _size; }
	const Point& topLeft() const { return _topLeft; }
	Point& topLeft() { return _topLeft; }
	const int value() const {return _value; }

public:
	//
	// stocked block enumerations
	//
	static Block ZhangFei;
	static Block CaoCao;
	static Block ZhaoYun;
	static Block MaChao;
	static Block GuanYu;
	static Block HuangZhong;
	static Block Soldier1;
	static Block Soldier2;
	static Block Soldier3;
	static Block Soldier4;

private:
	Point _topLeft;
	Size  _size;
	char _value;
};

template<class T> class Step{
public:
	Step(): _prev(0), _number(0){};
	Step(const T& value): _prev(0), _number(0), _value(value){}
	Step(Step* prev, const T& value): _prev(prev), _value(value){
		if(_prev)
			_number=_prev->_number+1;
	}

	const T& value() const   { return _value; }
	const Step* prev() const { return _prev; }
	const int number() const { return _number; }

	template<class StepPtrContainer>
	static bool findStep(StepPtrContainer coll, const T& value){
		struct TestEqual{
			TestEqual(const T& x):ref(x){}
			bool operator() (const Step* p) const{
				return p->value().isSameLayout(ref) || p->value().isMirrorLayout(ref);
			}
			const T& ref;
		};
		return std::find_if(coll.begin(), coll.end(), 
			TestEqual(value))!=coll.end();
	}

private:
	Step* _prev;
	T     _value;
	int   _number;
};

class Klotski{
public:
	static const int COL=4;
	static const int ROW=5;

	Klotski(){
		clearMap();
	}

	Klotski(const Klotski& ref): blocks(ref.blocks){ updateMap(); }

	const Klotski& operator=(const Klotski& ref){
		if( *this == ref ) return *this;
		blocks = ref.blocks;
		updateMap();
		return *this;
	}

	const bool operator == (const Klotski& ref) const { return blocks == ref.blocks; }

	void addBlock(const Block& block){
		blocks[block.value()] = block;
		updateMap(block);
	}

	const bool isSolved() const {
		//compare CaoCao
		std::map<char, Block>::const_iterator it = blocks.find('x');
		return Point(1, 3) == (it->second).topLeft();
	}

	bool isSameLayout(const Klotski& ref) const{
		// compare the layoutMap
		for(int y=0; y<ROW; ++y)
			for(int x=0; x<COL; ++x)
				if(layoutMap[y][x]!=ref.layoutMap[y][x])
					return false;
			
		return true;
	}

	bool isMirrorLayout(const Klotski& ref) const{
		for(int y=0; y<ROW; ++y)
			for(int x=0; x<COL; ++x)
				if(layoutMap[y][x]!=ref.layoutMap[y][COL-1-x])
					return false;
			
		return true;
	}

	template<class Container>
	std::list<Klotski> move(const Container& tried) const{
		std::list<Klotski> res;

		// left, right, up, down
		const int dx[4] = {-1, 1,  0, 0 };
		const int dy[4] = { 0, 0, -1, 1 };

		for(std::map<char, Block>::const_iterator it = blocks.begin();
			it!=blocks.end(); ++it)
		{
			for(int i=0; i<4; ++i){
				Point delta(dx[i], dy[i]);
				if(canMove(it->second, delta)){
					Klotski newLayout(*this);
					newLayout.moveBlock(it->second, delta);
					if(!Step<Klotski>::findStep(tried, newLayout))
						res.push_back(newLayout);
				}
			}
		}
		return res;
	}

	void print() const{
		for(int y=0; y<ROW; ++y){
			for(int x=0; x<COL; ++x)
				std::cout<<map[y][x]<<" ";
			std::cout<<"\n";
		}
	}

	void printLayout() const{
		for(int y=0; y<ROW; ++y){
			for(int x=0; x<COL; ++x){
				const Size& sz = layoutMap[y][x];
				int value = sz.width()*10+sz.height();
				std::cout<<value<<" ";
			}
			std::cout<<"\n";
		}
	}

private:
	bool canMove(const Block& block, const Point& delta) const{
		for(int i=0; i<block.size().height(); ++i)
			for(int j=0; j<block.size().width(); ++j)
			{
				int x = block.topLeft().x + j + delta.x;
				int y = block.topLeft().y + i + delta.y;
				
				if( x<0 || x>=COL || y<0 || y>=ROW) 
					return false;

				if(map[y][x] !='0' && map[y][x] !=block.value())
					return false;
			}

		return true;
	}

	const Klotski& moveBlock(const Block& block, const Point& delta){
		Block& ref = blocks[block.value()];
		clearMap(ref);
		ref.topLeft()+=delta;
		updateMap(ref);
		return *this;
	}

	void clearMap(){
		for(int y=0; y<ROW; ++y)
			for(int x=0; x<COL; ++x){
				map[y][x]='0';
				layoutMap[y][x]=Size(0,0);
			}
	}

	void clearMap(const Block& block){
		for(int i=0; i<block.size().height(); ++i)
			for(int j=0; j<block.size().width(); ++j){
				map[block.topLeft().y+i][block.topLeft().x+j] = '0';
				layoutMap[block.topLeft().y+i][block.topLeft().x+j] = Size(0, 0);
			}
	}

	void updateMap(){
		clearMap();
		for(std::map<char, Block>::iterator it = blocks.begin();
			it!=blocks.end(); ++it)
				updateMap(it->second);
	}

	void updateMap(const Block& block){
		for(int i=0; i<block.size().height(); ++i)
			for(int j=0; j<block.size().width(); ++j){
				map[block.topLeft().y+i][block.topLeft().x+j] = block.value();
				layoutMap[block.topLeft().y+i][block.topLeft().x+j] = block.size();
			}
	}

	char map[ROW][COL];
	Size layoutMap[ROW][COL];
	std::map<char, Block> blocks;
};

class KlotskiSolver{
public:
	typedef Step<Klotski> KlotskiStep;
	typedef std::deque<KlotskiStep*> StepQueue;
	typedef std::list<KlotskiStep*>  StepList;

	~KlotskiSolver(){
		for(StepQueue::iterator it = steps.begin();
			it!=steps.end(); ++it)
				delete (*it);

		for(StepList::iterator it = tried.begin();
			it!=tried.end(); ++it)
				delete (*it);
	}

	// must use width-first search!!
	bool run(const Klotski& layout){
		steps.push_back(new KlotskiStep(layout));

		while(!steps.empty()){
			KlotskiStep* step = steps.front();

			//
			std::cout<<"pick step: ["<<step->number()<<"]\n";
			step->value().print();
			std::cout<<"\n";
			//

			if(step->value().isSolved()){
				printSteps(step);
				return true;
			}
			else{
				steps.pop_front();
				tried.push_back(step);
				std::list<Klotski> nextLayouts = step->value().move(tried);

				for(std::list<Klotski>::iterator it=nextLayouts.begin();
					it!=nextLayouts.end(); ++it)
				{
					if(!KlotskiStep::findStep(steps, *it))
						steps.push_back(new KlotskiStep(step, *it));
				}
			}
		}
		return false;
	}

private:
	
	int printSteps(const Step<Klotski>* step){
		if(!step->prev()){
			std::cout<<"=============step:[0]==============\n";
			step->value().print();
			return 0;
		}
		else{
			int i=1+printSteps(step->prev());
			std::cout<<"=============step:["<<i<<"]==============\n";
			step->value().print();
			return i;
		}
	};

	StepQueue steps;
	StepList  tried;
};

//
// default: "Heng Dao Li Ma"
//
Block Block::ZhangFei   = Block(Point(0, 0), Size(1, 2), '1');
Block Block::CaoCao     = Block(Point(1, 0), Size(2, 2), 'x');
Block Block::ZhaoYun    = Block(Point(3, 0), Size(1, 2), '2');
Block Block::MaChao     = Block(Point(0, 2), Size(1, 2), '3');
Block Block::GuanYu     = Block(Point(1, 2), Size(2, 1), '4');
Block Block::HuangZhong = Block(Point(3, 2), Size(1, 2), '5');
Block Block::Soldier1   = Block(Point(0, 4), Size(1, 1), '6');
Block Block::Soldier2   = Block(Point(1, 3), Size(1, 1), '7');
Block Block::Soldier3   = Block(Point(2, 3), Size(1, 1), '8');
Block Block::Soldier4   = Block(Point(3, 4), Size(1, 1), '9');

class TestKlotski : public TestCase
{
	void test(){
		TestScript<TestKlotski>::begin(this)
			<<&TestKlotski::testLayout
			<<&TestKlotski::testTarget
			<<&TestKlotski::testMove
			<<&TestKlotski::testEasy
			<<&TestKlotski::testRun
			<<end;
	}

	void testLayout(){
		Klotski game = createLayout1();
		game.print();
	}

	void testTarget(){
		std::cout<<"=============test target==============\n";
		Klotski game;
		game.addBlock(Block::CaoCao.at(Point(1, 3)));
		KlotskiSolver solver;
		solver.run(game);
	}

	void testMove(){
		Klotski game = createLayout1();
		std::list<Step<Klotski>*> empty;
		std::list<Klotski> res = game.move(empty);
		assertTrue(!res.empty());
		int i=0;
		for(std::list<Klotski>::iterator it=res.begin(); it!=res.end(); ++it){
			std::cout<<"===========candidate["<<i++<<"]=============\n";
			it->print();
		}
	}

	void testEasy(){
		std::cout<<"=============test solver run==============\n";
		Klotski game = createLayout2();
		game.print();
		KlotskiSolver solver;
		solver.run(game);
	}

	void testRun(){
		std::cout<<"=============test solver run==============\n";
		Klotski game = createLayout1();
		KlotskiSolver solver;
		solver.run(game);
	}

private:
	Klotski createLayout1(){
		std::cout<<"=============Hua Rong Path[Heng Dao Li Ma]===========\n";
		Klotski game;
		game.addBlock(Block::ZhangFei);
		game.addBlock(Block::CaoCao);
		game.addBlock(Block::ZhaoYun);
		game.addBlock(Block::MaChao);
		game.addBlock(Block::GuanYu);
		game.addBlock(Block::HuangZhong);
		game.addBlock(Block::Soldier1);
		game.addBlock(Block::Soldier2);
		game.addBlock(Block::Soldier3);
		game.addBlock(Block::Soldier4);
		return game;
	}

	Klotski createLayout2(){
		std::cout<<"=============Hua Rong Path[Very Easy]===========\n";
		Klotski game;
		game.addBlock(Block::ZhangFei.at(Point(0, 0)));
		game.addBlock(Block::CaoCao.at(Point(1, 2)));
		game.addBlock(Block::ZhaoYun.at(Point(3, 0)));
		game.addBlock(Block::MaChao.at(Point(0, 2)));
		game.addBlock(Block::GuanYu.at(Point(1, 0)));
		game.addBlock(Block::HuangZhong.at(Point(3, 2)));
		game.addBlock(Block::Soldier1.at(Point(0, 4)));
		game.addBlock(Block::Soldier2.at(Point(1, 1)));
		game.addBlock(Block::Soldier3.at(Point(2, 1)));
		game.addBlock(Block::Soldier4.at(Point(3, 4)));
		return game;
	}
};
#endif
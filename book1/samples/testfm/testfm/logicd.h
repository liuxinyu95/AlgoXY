#ifndef _LOGIC_D_
#define _LOGIC_D_

#include "testcase.h"
#include "testscript.h"
#include "testcaller.h"

const char* ways[]={"road", "path", "mountain", "river", "bridge"};
const static char* targets[]={"castle", "dragon", "lion", "marsh", "nothing"};
enum WAY{ ROAD=0, PATH, MOUNTAIN, RIVER, BRIDGE };

bool meet(int target, int way){
	return target == way;
}

void judge(){
	bool onWay[5];
	for(int i=0; i<5; ++i)
		onWay[i]=false;

	for(int castle=0; castle<5; ++castle){
		onWay[castle]=true;
		for(int dragon=0; dragon<5; ++dragon){
			if(onWay[dragon]) continue;
			onWay[dragon]=true;
			for(int lion=0; lion<5; ++lion){
				if(onWay[lion]) continue;
				onWay[lion]=true;
				for(int marsh=0; marsh<5; ++marsh){
					if(onWay[marsh]) continue;

					int nothing = (1+2+3+4)-castle-dragon-lion-marsh;

					//
					if( meet(lion, MOUNTAIN) == meet(marsh, RIVER) ) continue;
					if( meet(castle, ROAD) == meet(nothing, BRIDGE) ) continue;
					if( meet(nothing, MOUNTAIN) == meet(dragon, RIVER) ) continue;
					if( meet(marsh, PATH) == meet(lion, BRIDGE) ) continue;

					std::cout<<"castle\t"<<ways[castle]<<"\n"
						<<"dragon\t"<<ways[dragon]<<"\n"
						<<"lion\t"<<ways[lion]<<"\n"
						<<"marsh\t"<<ways[marsh]<<"\n"
						<<"nothing\t"<<ways[nothing]<<"\n";
				}
				onWay[lion]=false;
			}
			onWay[dragon]=false;
		}
		onWay[castle]=false;
	}
}

class Match{
public:
	enum NATION {England=0, Germany, Brazil, France};

	Match(const int winner[], int loser[]){
		memset(table, 0, sizeof(int)*4*4);
		for(int i=0; i<4; ++i){
			table[i][winner[i]]=table[i][loser[i]]=1;
		}
	}

	int plays(int nation){
		int res=0;
		for(int i=0; i<4; ++i)
			res+=table[i][nation];
		return res;
	}

	bool hasAbsent(int g1, int g2){
		for(int nation=0; nation<4; ++nation)
			if(table[g1][nation]==0 && table[g2][nation]==0)
				return true;
		return false;
	}

	bool sameGame(int g1, int g2){
		for(int i=0; i<4; ++i)
			if(table[g1][i] != table[g2][i])
				return false;
		return true;
	}

	bool noSameGame(){
		for(int i=0; i<4; ++i)
			for(int j=0; j<4; ++j)
				if((i!=j) && sameGame(i, j))
					return false;
		return true;
	}

	bool hasPlayed(int nation1, int nation2){
		for(int i=0; i<4; ++i)
			if(table[i][nation1] && table[i][nation2])
				return true;
		return false;
	}

	void print(){
		const char* nation[]={"England", "Germany", "Brazil", "France"};

		for(int i=0; i<4; ++i){
			std::cout<<i+1<<":\t";
			for(int j=0; j<4; ++j)
				if(table[i][j])
					std::cout<<nation[j]<<"\t";
			std::cout<<"\n";
		}
	}

private:
	int table[4][4];
};



void worldCup(){
	const int winner[]={Match::England, Match::Brazil, Match::Germany, Match::France};
	int loser[4];

	for(loser[0]=0; loser[0]<4; ++loser[0]){
		if(loser[0]==winner[0]) continue;

		for(loser[1]=0; loser[1]<4; ++loser[1]){
			if(loser[1]==winner[1]) continue;

			for(loser[2]=0; loser[2]<4; ++loser[2]){
				if(loser[2]==winner[2]) continue;

				for(loser[3]=0; loser[3]<4; ++loser[3]){
					if(loser[3]==winner[3]) continue;

					//judge
					Match game(winner, loser);
					if(game.plays(Match::Germany)!=2 || game.plays(Match::France)!=2)
						continue;
					if(game.plays(Match::England)!=3 && game.plays(Match::Brazil)!=3)
						continue;

					if(game.hasAbsent(1,2))
						continue;

					if(!game.noSameGame())
						continue;

					if(game.hasPlayed(Match::Germany, Match::Brazil))
						continue;

					//output
					game.print();
				}
			}
		}
	}
}

struct TestLogicD: public TestCase{
	void test(){
		TestScript<TestLogicD>::begin(this)
			<<&TestLogicD::testLiarIsland
			<<end;
	}

	void testLiarIsland(){
		judge();
		worldCup();
	}
};
#endif
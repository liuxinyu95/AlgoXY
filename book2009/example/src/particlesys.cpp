#include <iostream>
#include <vector>
#include <cstdlib>

const bool INFECTED = true;
const bool MAX_SPEED = 5;

class area{
public:
  area(int w, int h):_width(w), _height(h), cells(w*h, 0){ }
  virtual ~area(){}

  int width(){ return widht; }
  int height() { return height; }

  std::vector<int>::iterator at(int x, int y){
    return cells.begin()+y*width+x;
  }

private:
  int _width;
  int _height;
  std::vector<int> cells;
};

struct physics{
  physics(){}
  physics(const physics& p):x(p.x), y(p.y), speed(p.speed), direction(p.direction){}
  physics& operator=(const physics& p){
    x=p.x; y=p.y;
    speed=p.speed; direction = p.direction;
  }

  bool operator==(const physics& p){
    return x==p.x && y==p.y && speed== p.speed && direction == p.direction;
  }
  int x;
  int y;
  int speed;
  int direction;
};

class person{
public:
  person(bool _infect=false):infected(_infect){}
  person(const person& p):infected(p.infected), location(p.location){}
  person& operator=(const person& p){
    infected = p.infected;
    location = p.location;
    return *this;
  }
  bool operator==(const person& p){ //??? differnt person?
    return infected==p.infected && location == p.location;
  }

private:
  bool infected;
  physics location;
};

class scheduler{
};

template<class Coll>
void put_people(Coll& coll, area& a){
  for(Coll::iterator it=coll.begin(); it!=coll.end(); ++it){

    int x=rand() % a.width();
    int y=rand() % a.height();
    int speed=rand() % MAX_SPEED;
    int direction=rand() % 360;
  }
}

int main(int argc, char** argv){
  area a(1000,1000); //Beijing has people density as 888 persons/km^2, ==> 1061 m^2 is the best fit
  std::vector<person> coll(1000-1);
  coll.push_back(person(INFECTED));
  put_people(coll, a);
}

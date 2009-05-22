#include <iostream>
#include <vector>
#include <cstdlib>
#include <cmath>

const bool INFECTED = true;
const int  MAX_SPEED = 50; // walking speed: 50 [m/min]
const double dt = 1;       // 1 min
const double pi = 3.141592654;

class area{
public:
  area(int w, int h):_width(w), _height(h), cells(w*h, 0){ }
  virtual ~area(){}

  int width(){ return widht; }
  int height() { return height; }

  std::vector<int>::iterator at(int x, int y){
    return cells.begin()+y*width+x;
  }

  void reset(){
    cells.swap(std::vector<int>(w*h, 0));
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

  void move(double dt){
    x+=static_cast<int>(static_cast<double>(speed)*dt*cos(static_cast<double>(direction)/180.0*pi));
    y+=static_cast<int>(static_cast<double>(speed)*dt*sin(static_cast<double>(direction)/180.0*pi));
  }

  int x;
  int y;
  int speed;
  int direction;
};

class person{
public:
  person(bool x=false):_infected(x){}
  person(const person& p):_infected(p._infected), _loc(p._loc){}
  person& operator=(const person& p){
    _infected = p._infected;
    _loc = p._loc;
    return *this;
  }
  bool operator==(const person& p){ //??? differnt person?
    return _infected==p._infected && _loc == p._loc;
  }

  physics location() const{ return _loc; }
  void set_location(const physics& l){ _loc=l; }
  bool infected() const { return _infected; }
  void set_infected(bool x) { _infected = x; }

  void move_inside(area& a, double dt){
    _loc.move(dt);
    if(_loc.x<0 || _loc.x>a.width())
      _loc.degree=(180-_loc.degree+360)%360;
    if(_loc.y<0 || _loc.y>a.height())
      _loc.degree=(-_loc.degree+360)%360;
    if(_loc.x<0)
      _loc.x=-_loc.x;
    if(_loc.x>a.width())
      _loc.x=2*a.width()-_loc.x;
    if(_loc.y<0)
      _loc.y=-locatoin.y;
    if(_loc.y>a.height())
      _loc.y=2*a.height()-_loc.y;
  }

private:
  bool _infected;
  physics _loc;
};

class scheduler{
public:
  static scheduler& inst(){
    static scheduler _inst;
    return _inst;
  }

  void setup(int w, int h, int n){
    a=area(w, h);
    people=Population(n-1);
    people.push_back(person(INFECTED));
    put_people(people, a);
  }
  void start(){
    run();
  }
private:
  void run(){
    a.reset();
    move();
    infect();
  }

  void move(){
    for(Population::iterator it=people.begin(); it!=people.end(); ++it){
      it->move_inside(a, dt);   // change to foreach
      if(it->infected())
        *(a.at(it->location().x, it->location().y))=1;
    }
  }

  void infect(){
    for(Population::iterator it=people.begin(); it!=people.end(); ++it)
      if(*(a.at(it->location().x, it->location().y)))
        it->set_infected(true);   // change to foreach
  }

  area a;
  typedef std::vector<person> Population;
  Population people;
};

template<class Coll>
void put_people(Coll& coll, area& a){
  for(Coll::iterator it=coll.begin(); it!=coll.end(); ++it){
    it->set_location(physics(rand()%a.width(), rand()%a.height(),
                             rand()%MAX_SPEED, rand()%360));
  }
}

int main(int argc, char** argv){
  //Beijing has people density as 888 persons/km^2, ==> 1061 m^2 is the best fit
  scheculer::inst().setup(1061, 1061, 1000);
  scheduler::inst().start();
}

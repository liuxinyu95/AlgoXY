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

    void set_location(const physics& l){
        location=l;
    }

private:
  bool infected;
  physics location;
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
        for
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

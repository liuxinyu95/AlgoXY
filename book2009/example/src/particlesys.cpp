#include <iostream>
#include <list>
#include <cstdlib>
#include <cmath>
#include <iterator>
#include <fstream>


const int  MAX_SPEED = 50; // walking speed: 50 [m/min]
const double delta = 1;       // 1 min
const double pi = 3.141592654;

template<bool HasHospital>
struct config{
  const static bool _hasHospital = HasHospital;
};

struct area{
public:
  area(int w, int h):x0(0), y0(0), width(w), height(h){
    cells=new char[w*h];
    reset();
  }
  area(int x, int y, int w, int h):x0(x), y0(y), width(w), height(h), cells(0){ }

  virtual ~area(){ delete cells; }

  char* at(int x, int y){ return cells+y*width+x; }

  void reset(){ memset(cells, 0, width*height); }      


  template<class Loc>
  bool contains(const Loc& l){
    return l.x-x0 >=0 && l.x-x0 < width && l.y-y0 >=0 && l.y-y0 < height;
  }

  int x0;
  int y0;
  int width;
  int height;

private:
  char* cells;
};

struct physics{
  physics(){}
  physics(int _x, int _y, int _s, int _d):x(_x), y(_y), speed(_s), direction(_d){}
  physics(const physics& p):x(p.x), y(p.y), speed(p.speed), direction(p.direction){}
  physics& operator=(const physics& p){
    x=p.x; y=p.y;
    speed=p.speed; direction = p.direction;
  }

  bool operator==(const physics& p){
    return x==p.x && y==p.y && speed == p.speed && direction == p.direction;
  }

  void move(double dt){
    x+=static_cast<int>(static_cast<double>(speed)*dt*cos(static_cast<double>(direction)/180.0*pi));
    y+=static_cast<int>(static_cast<double>(speed)*dt*sin(static_cast<double>(direction)/180.0*pi));
    speed = rand() % MAX_SPEED;
  }

  int x;
  int y;
  int speed;
  int direction;
};

template<int DELITESCENCE = 0>
class person{
public:
  person(bool x=false):_infected(x), _delitescence(0){}
  person(const person& p):_infected(p._infected), _delitescence(p._delitescence),_loc(p._loc){}
  person& operator=(const person& p){
    _infected = p._infected;
    _delitescence = p._delitescence;
    _loc = p._loc;
    return *this;
  }

  physics location() const{ return _loc; }
  void set_location(const physics& l){ _loc=l; }
  bool infected() const { return _infected; }
  void set_infected(bool x) { _infected = x; }
  bool ill() const { return _infected && _delitescence > DELITESCENCE; }
  void inc_delitescence() { ++_delitescence; }

  void move_inside(area& a, double dt){
    _loc.move(dt);
    if(_loc.x-a.x0 < 0 || _loc.x-a.x0 >= a.width)
      _loc.direction = (180 - _loc.direction + 360) % 360;
    if(_loc.y-a.y0 < 0 || _loc.y-a.y0 >= a.height)
      _loc.direction = (-_loc.direction + 360) % 360;
    if(_loc.x-a.x0 < 0)
      _loc.x=2*a.x0-_loc.x;
    if(_loc.x-a.x0 >= a.width)
      _loc.x=2*(a.x0+a.width-1)-_loc.x;
    if(_loc.y-a.y0 < 0)
      _loc.y=2*a.y0-_loc.y;
    if(_loc.y-a.y0 >= a.height)
      _loc.y=2*(a.y0+a.height-1)-_loc.y;
  }

  void move_to(const area& a){
    if(a.x0 == _loc.x)
      _loc.direction = a.y0 > _loc.y ? 90 : 270;
    else
      _loc.direction = static_cast<int>(atan(static_cast<double>(a.y0-_loc.y)/
                                             static_cast<double>(a.x0-_loc.x))/pi*180.0);
  }

private:
  bool _infected;
  int  _delitescence;
  physics _loc;
};

template<
  bool HasHospital = false, 
  int  INFECT_PROBABILITY = 50,
  int  DELITESCENCE = 0,
  int  EXIT_RATE = 90
>
class scheduler{
  typedef person<DELITESCENCE> Person;
  typedef std::list<Person*> Population;
  const static bool INFECTED = true;

public:
  scheduler(int w, int h, int n):hospital(0){ 
    a=new area(w, h);
    for(int i=0; i<n-1; ++i)
      people.push_back(new Person());
    people.push_back(new Person(INFECTED));
    put_people(people, *a);
    n_infected = 1;    
  }

  ~scheduler(){
    delete a;
    delete hospital;
    for(typename Population::iterator it=people.begin(); it!=people.end(); ++it)
      delete *it;
  }

  scheduler& set_hospital(int x, int y, int w, int h){
    hospital=new area(x, y, w, h);
    return *this;
  }

  void run(){
    for(unsigned int tm=0; n_infected < people.size()*EXIT_RATE/100; ++tm){
      a->reset();
      move(config<HasHospital>());
      infect();
      std::cout<<"time "<<tm/60<<":"<<tm%60<<" "
               <<n_infected<<"/"<<people.size()<<" are infected\r";
      diagram.push_back(n_infected);
    }
    write_diagram();
  }

private:
  void move(config</*HasHospital=*/true>){
    for(typename Population::iterator it=people.begin(); it!=people.end(); ++it){
      if((*it)->ill()){
        if(hospital->contains((*it)->location()))
          (*it)->move_inside(*hospital, delta);
        else{
          (*it)->move_to(*hospital);
          (*it)->move_inside(*a, delta);
        }
      }
      else
        (*it)->move_inside(*a, delta);
      if((*it)->infected()){
        *(a->at((*it)->location().x, (*it)->location().y))=1;
        (*it)->inc_delitescence();
      }
    }
  }

  void move(config</*HasHospital=*/false>){
    for(typename Population::iterator it=people.begin(); it!=people.end(); ++it){
      (*it)->move_inside(*a, delta);
      if((*it)->infected())
        *(a->at((*it)->location().x, (*it)->location().y))=1;
    }
  }

  void infect(){
    for(typename Population::iterator it=people.begin(); it!=people.end(); ++it)
      if(!(*it)->infected() &&
         *(a->at((*it)->location().x, (*it)->location().y))==1 &&
         rand()%100 >= INFECT_PROBABILITY){
        n_infected++;
        (*it)->set_infected(true);
      }
  }

  template<class Coll>
  void put_people(Coll& coll, area& a){
    for(typename Coll::iterator it=coll.begin(); it!=coll.end(); ++it){
      (*it)->set_location(physics(rand()%a.width, rand()%a.height,
                                  rand()%MAX_SPEED, rand()%360));
    }
  }

  void write_diagram(){
    std::ofstream file("diagram.csv");
    std::copy(diagram.begin(), diagram.end(), std::ostream_iterator<int>(file, "\n"));
  }

  area* a;
  area* hospital;
  Population people;
  int n_infected;
  std::list<int> diagram;
};

int main(int argc, char** argv){
  //Beijing has people density as 888 persons/km^2, ==> 1061 m^2 is the best fit
  for(int i=0;i!=-1;){
    std::cout<<"\n1, simple epidemic simulation;"
             <<"\n2, with probability;"
             <<"\n3, with hospital;"
             <<"\n4, with delitescence;"
             <<"\n-1, exit.\n[1,2,3,4,-1]==>";
    std::cin>>i;
    switch(i){
    case 1:
      scheduler</*HasHospital=*/false, /*INFECT_PROBABILITY=*/0>(1061, 1061, 1000).run();
      break;
    case 2:
      scheduler<>(1061, 1061, 1000).run();
      break;
    case 3:
      scheduler<true, 50, /*DELITESCENCE=*/0, /*EXIT_RATE=*/2>(1061, 1061, 1000).set_hospital(500, 500, 50, 50).run();
      break;
    case 4:
      scheduler<true, 50, /*DELITESCENCE=*/3*24*60>(1061, 1061, 1000).set_hospital(500, 500, 50, 50).run();
      break;
    case -1:
    default:
      break;
    }
  }
}

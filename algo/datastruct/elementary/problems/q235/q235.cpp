/*
 * q235.cpp, 
 * Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <iostream>
#include <deque>
#include <algorithm>
#include <ctime>

typedef unsigned long Integer;
using namespace std;

Integer get_q235(int n){
  if(n==1) 
    return 1;
  deque<Integer> Q2, Q3, Q5;
  Q2.push_back(2);
  Q3.push_back(3);
  Q5.push_back(5);
  Integer x;
  while(n>1){
    x = min(min(Q2.front(), Q3.front()), Q5.front());
    if(x==Q2.front()){
      Q2.pop_front();
      Q2.push_back(x*2);
      Q3.push_back(x*3);
      Q5.push_back(x*5);
    }
    else if(x==Q3.front()){
      Q3.pop_front();
      Q3.push_back(x*3);
      Q5.push_back(x*5);
    }
    else{
      Q5.pop_front();
      Q5.push_back(x*5);
    }
    --n;
  }
  return x;
}

typedef Integer (*Fun)(int);

double test(Fun f, int n){
  clock_t start, end;
  start = clock();
  Integer x = f(n);
  end=clock();
  cout<<"the "<<n<<"th number = "<<x<<"\n";
  cout<<"start="<<start<<"\tend="<<end<<"\n";
  return ((double) (end - start)) * 1000000.0 / CLOCKS_PER_SEC;
}

int main(){
  cout.precision(10);
  cout<<"q235 method time: "<<test(get_q235, 15000)<<"[ms]\n";
}

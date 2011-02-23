/*
 * measure.h, Measure tool
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

#include <stdio.h>
#include "randlist.h"
#include "measure.h"

#define N 10000000
int ids[N];

void init_ids(){
  int i;
  for(i=0; i<N; ++i)
    ids[i] = i;
}

int brute_force(int* xs, int n){
  int i, j;
  for(i=0; ; ++i){
    int in = 0;
    for(j=0; j<n; ++j)
      if(xs[j] == i){
	in = 1;
	break;
      }
    if(!in) return i;
  }
}

int fire_brute_force(int x, int y){
  return brute_force((int*)x, y);
}

double measure_brute_force(){
  double t, tm = 0.0;
  int i;
  printf("start measure brute force method...\n");
  for(i = 0; i < 100; ++i){
    shuffle(ids, N);
    t = measure(fire_brute_force, (int)ids, N-1);
    //printf("one search in %f[s]\n", t);
    tm = tm + t;
  }
  return tm / 100.00;
}

int main(){
  printf("average time of brute-force method: %f[s]\n",
	 measure_brute_force());
  //average time of brute-force method: 0.044870[s]
  return 0;
}

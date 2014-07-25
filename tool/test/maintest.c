/*
 * maintest.c, main test program for helper tools
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

void print_list(int* xs, int n){
  int i;
  for(i=0; i<n; ++i)
    printf("%d, ", xs[i]);
  printf("\n");
}

void test_randlist(){
  int i, ns[100], xs[10];
  for(i=0; i<100; ++i)
    ns[i] = i;
  shuffle(ns, 100);
  printf("shuffle: ");
  print_list(ns, 100);

  sample(ns, 100, 10);
  printf("sample: ");
  print_list(ns, 10);

  gen_knuth(xs, 10, 100);
  printf("gen_knuth: ");
  print_list(xs, 10);
}

int fire(int x, int y){
  shuffle((int*)x, y);
  return 0;
}

void test_measure(){
  const int n = 10000;
  int i, ns[n];
  for(i=0; i<n; ++i){
    ns[i] = i;
  }

  double tm = 0.0;
  for(i=0; i<100; ++i)
    tm += measure(fire, (int)ns, n);
  printf("average time: %f[s]", tm/100.0);
}

int main(int argc, char** argv){
  test_randlist();
  test_measure();
  return 0;
}

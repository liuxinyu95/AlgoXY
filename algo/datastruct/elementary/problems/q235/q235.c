/*
 * q235.h, 
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
#include <time.h> //for performance measurement

typedef int (*Fun)(int);

int valid(int x){
  while( x % 2 == 0)
    x /= 2;
  while( x % 3 == 0)
    x /= 3;
  while( x % 5 == 0)
    x /= 5;
  return x==1;
}

int brute_force(int n){
  int i;
  for(i=1; ;++i)
    if(valid(i) && --n==0)
      return i;
}

double test(Fun f, int n){
  clock_t start, end;
  start = clock();
  int x = f(n);
  end=clock();
  printf("the %dth number = %d\n", n, x);
  return ((double) (end - start)) /CLOCKS_PER_SEC;
}

struct queue{
  int head;
  int tail;
  int max_size;
  int* data;
};

void init_queue(struct queue* q, int max_size){
  q->data = (int*)malloc(sizeof(int)*max_size);
  q->head = q->tail = 0;
  q->max_size = max_size;
};

void release_queue(struct queue* q){
  free(q->data);
}

// skip the empty error handling
int pop(struct queue* q){
  int x = q->data[head];
  ++head;
  if(head >=max_size)
    head -= max_size;
  return x;
}

// ordered unique push
void ou_push(struct queue* q, int x){
}

int main(){
  printf("brute force method time: %f[s]\n", test(brute_force, 15000));
  return 0;
}

/** some performance data:
 * 1500th number
 * brute force method time: 40.391000[s]
 *
 * 15,000 number
 * brute force: > 10 [min]
 */

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

typedef unsigned long Integer;

typedef Integer (*Fun)(int);

int tm;

int valid(Integer x){
  while( x % 2 == 0)
    x /= 2;
  while( x % 3 == 0)
    x /= 3;
  while( x % 5 == 0)
    x /= 5;
  return x==1;
}

Integer brute_force(int n){
  Integer i;
  for(i=1; ;++i)
    if(valid(i) && --n==0)
      return i;
}

/* simulate queue with array*/
struct queue{
  int head;
  int tail;  /*range [head, tail)*/
  int max_size;
  Integer* data;
};

void init_queue(struct queue* q, int max_size){
  q->data = (Integer*)malloc(sizeof(Integer)*max_size);
  q->head = 0;
  q->tail = 0;
  q->max_size = max_size;
};

void release_queue(struct queue* q){
  free(q->data);
}

Integer get_at(struct queue* q, int i){
  ++tm;
  while(i>=q->max_size)
    i -= q->max_size;
  return q->data[i];
}

void set_at(struct queue* q, int i, Integer x){
  ++tm;
  while(i>=q->max_size)
    i -= q->max_size;
  q->data[i] = x;
}

void print_q(struct queue* q){
  printf("queue: head = %d, tail=%d, max-size=%d\n", q->head, q->tail, q->max_size);
  int i;
  for(i=q->head; i<q->tail; ++i)
    printf("%d, ", get_at(q, i));
  printf("end\n");
}

// skip the empty error handling
Integer dequeue(struct queue* q){
  Integer x = get_at(q, q->head);
  if(x<0){
    print_q(q);
    exit(-1);
  }
  ++q->head;
  return x;
}

void insert(struct queue* q, int i, Integer x){
  int j;
  for(j=q->tail; j>i; --j)
    set_at(q, j, get_at(q, j-1));
  set_at(q, i, x);
  ++q->tail;
}

// ordered unique enqueue
void ou_enqueue(struct queue* q, Integer x){
  /**if(x <0)
     return; */ //error due to overflow
  int i=q->head;
  while(i<q->tail && get_at(q, i) < x)
    ++i;
  if(i<q->tail && x == get_at(q, i))
    return;  /*duplicate, do nothing*/
  insert(q, i, x);
}

Integer get_number_q(int n){
  Integer x;
  struct queue q;
  init_queue(&q, n*5);
  ou_enqueue(&q, 1);
  while(n>0){
    x = dequeue(&q);
    ou_enqueue(&q, x*2);
    ou_enqueue(&q, x*3);
    ou_enqueue(&q, x*5);
    --n;
  }
  release_queue(&q);
  return x;
}

int verify(Fun f){
  /*verify by using brute_force method*/
  int i;
  for(i=1; i<200; ++i)
    if(f(i)!=brute_force(i)){
      printf("Fail\n");
      return 0;
    }
  printf("OK\n");
  return 1;
}

double test(Fun f, int n){
  clock_t start, end;
  start = clock();
  Integer x = f(n);
  end=clock();
  printf("the %dth number = %d\n", n, x);
  return ((double) (end - start)) /CLOCKS_PER_SEC;
}

/*usage: app > log.csv*/
int performance(Fun f){
  int i;
  for(i=1; i<1500; ++i){
    tm = 0;
    f(i);
    printf("%d, %d, %d\n", i, i*i, tm);
  }
}

int main(){
  //printf("brute force method time: %f[s]\n", test(brute_force, 15000));
  printf("verify 1 queue solution: %d\n", verify(get_number_q));
  printf("1 queue method time: %f[s]\n", test(get_number_q, 1500));
  //performance(get_number_q);
  return 0;
}

/** some performance data:
 * 1500th number
 * brute force method time: 40.391000[s]
 *
 * 15,000 number
 * brute force: > 10 [min]
 */

/*
 * queue.c, Conventional queue implementation
 * Copyright (C) 2012, Liu Xinyu (liuxinyu95@gmail.com)

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

#include <stdlib.h>
#include <stdio.h>

/* For verification purpose only */
#include <assert.h>

#define BIG_RAND() (rand() % 10000)
#define ODD(x)     ((x) & 1)
/* End of verification purpose only part */

typedef int Key;

/*
 * Queue realized by Linked list.
 * Using a satinel for simplification
 */
struct Node{
  Key key;
  struct Node* next;
};

Key key(struct Node* x){ return x->key; }

struct Queue{
  struct Node *head, *tail;
};

/*
 * Insert new new at the end, while remove node from the front.
 *  We can't achieve both O(1) if insert before front and remove at the end.
 */

struct Queue* create(){
  struct Queue* q = (struct Queue*)malloc(sizeof(struct Queue));
  struct Node* s = (struct Node*)malloc(sizeof(struct Node));
  s->next = NULL;
  q->head = q->tail = s; /* sentinel */
  return q;
}

int empty(struct Queue* q){
  return q->head == q->tail;
}

void destroy(struct Queue* q){
  struct Node* p;
  while(q->head){
    p = q->head;
    q->head = q->head->next;
    free(p);
  }
  free(q);
}

/* O(1) by appending */
struct Queue* enqueue(struct Queue* q, Key x){
  struct Node* p = (struct Node*)malloc(sizeof(struct Node));
  p->key = x;
  p->next = NULL;
  q->tail->next = p;
  q->tail = p;
  return q;
}

struct Node* head(struct Queue* q){ return q->head->next; }

/* 
 * O(1) by removing from front 
 *   Assume queue isn't empty.
 */

Key dequeue(struct Queue* q){
  struct Node* p = head(q);
  Key x = key(p);
  q->head->next = p->next;
  if(q->tail == p)
    q->tail = q->head;
  free(p);
  return x;
}

/* Queue realized by circular buffer */

struct QueueBuf{
  Key* buf;
  int head, tail, size;
};

struct QueueBuf* createQ(int max){
  struct QueueBuf* q = (struct QueueBuf*)malloc(sizeof(struct QueueBuf));
  q->buf = (Key*)malloc(sizeof(Key)*max);
  q->size = max;
  q->head = q->tail = 0;
  return q;
}

void destroyQ(struct QueueBuf* q){
  free(q->buf);
  free(q);
}

int fullQ(struct QueueBuf* q){
  return q->tail + 1 == q->head || 
         q->tail + 1 - q->size == q->head;
}

int emptyQ(struct QueueBuf* q){
  return q->head == q->tail;
}

/* O(1) append to tail */
void enQ(struct QueueBuf* q, Key x){
  if(!fullQ(q)){
    q->buf[q->tail++] = x;
    q->tail -= q->tail< q->size ? 0 : q->size;
  }
}

/* Assume queue isn't empty */
Key headQ(struct QueueBuf* q){
  return q->buf[q->head];
}

/* O(1) remove from head */
Key deQ(struct QueueBuf* q){
  Key x = headQ(q);
  q->head++;
  q->head -= q->head< q->size ? 0 : q->size;
  return x;
}

/* Testing */

void test_queue(){
  int m = 100;
  int i, n, x;
  struct Queue* q1;
  struct QueueBuf* q2;
  while(m--){
    n = BIG_RAND();
    q1 = create();
    q2 = createQ(n);
    for(i=0; i<n; ++i){
      x = BIG_RAND();
      if(ODD(x)){
	enqueue(q1, x);
	enQ(q2, x);
      }
      else if(! (empty(q1) || emptyQ(q2)))
	assert(dequeue(q1) == deQ(q2));
    }
    destroy(q1);
    destroyQ(q2);
  }
}

int main(){
  test_queue();
  return 0;
}

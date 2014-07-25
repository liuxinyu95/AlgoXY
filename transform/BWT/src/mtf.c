/*
 * mtf.c, Move-to-front transform
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

#include <stdlib.h>
#include <stdio.h>  /*for testing purpose only*/
#include <string.h> /*for testing purpose only*/

#define M 256
typedef unsigned char byte;

struct node{
  byte value;
  struct node* prev;
  struct node* next;
};

struct node tab[M];

/* return the head of the linked-list*/
struct node* build_table(){
  int i;
  for(i=0; i<M; ++i){
    tab[i].value = (byte)i;
    tab[i].prev = &tab[(M+i-1) % M];
    tab[i].next = &tab[(i+1) % M];
  }
  return tab;
}

void move_to_front(struct node** head, struct node* p){
  if(*head != p){
    p->prev->next = p->next;
    p->next->prev = p->prev;
    p->next = *head;
    p->prev = 0;
    (*head)->prev = p;
    *head = p;
  }
}

byte mtf_lookup(struct node** head, byte x){
  struct node* p = *head;
  int i=0;
  while(1){
    if(p->value == x){
      move_to_front(head, p);
      return i;
    }
    ++i;
    p = p->next;
  }
}

void mtf(byte* xs, byte* ys, int n){
  int i;
  struct node* lst = build_table();
  for(i=0; i<n; ++i)
    ys[i] = mtf_lookup(&lst, xs[i]);
}

byte imtf_lookup(struct node** head, byte x){
  struct node* p = *head;
  byte c;
  while(x--)
    p = p->next;
  c = p->value;
  move_to_front(head, p);
  return c;
}

void imtf(byte* xs, byte* ys, int n){
  int i;
  struct node* lst = build_table();
  for(i=0; i<n; ++i)
    ys[i] = imtf_lookup(&lst, xs[i]);
}

/*testing*/

void prints(byte* xs, int n){
  int i;
  for(i=0; i<n; ++i)
    printf("%d, ", xs[i]);
}

int assert_eq(byte* xs, byte* zs, int n){
  int i;
  for(i=0; i<n; ++i)
    if(xs[i] != zs[i]){
      printf("Fail!\n");
      return 0;
    }
  printf("OK.\n");
  return 1;
}

void test(byte* xs, int n){
  byte* ys = (byte*)malloc(sizeof(byte)*n);
  byte* zs = (byte*)malloc(sizeof(byte)*n);
  printf("\ndata: ");
  printf("%s\n", xs);
  mtf(xs, ys, n);
  printf("code: ");
  prints(ys, n);
  imtf(ys, zs, n);
  printf("\ndecode: ");
  printf("%s\n", zs);
  assert_eq(xs, zs, n);
  free(ys);
  free(zs);
}

int main(int argc, char** argv){
  int i;
  const char* xs[] = {"banana", "mississippi", "cocoa"};
  for(i=0; i<sizeof(xs)/sizeof(const char*); ++i)
    test((byte*)xs[i], strlen(xs[i])+1);
  return 0;
}

/*
 * fib_helper.h, Helper functions for debug purpose only.
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

#ifndef __FIB_HELPER__
#define __FIB_HELPER__

void print_tr(struct node* t){
  struct node* x;
  if(t){
    printf("%d", t->key);
    if(t->mark)
      printf("*");
    if(t->children){
      printf("-(");
      x=t->children;
      do{
	print_tr(x);
	printf(", ");
	x = x->next;
      }while(x!=t->children);
      printf(")");
    }
  }
}

void print_heap(struct FibHeap* h){
  struct node* x;
  if(h->roots){
    x=h->roots;
    do{
      if(x==h->minTr)
	printf("[min]");
      print_tr(x);
      printf("-->");
      x = x->next;
    }while(x != h->roots);
  }
  printf(".\n");
}

void print_lst(const int* xs, int n){
  int i;
  for(i=0; i<n; ++i)
    printf("%d, ", xs[i]);
  printf("\n");
}

#endif 


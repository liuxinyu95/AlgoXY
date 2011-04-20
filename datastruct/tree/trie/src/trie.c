/*
 * trie.c, Alphabetic Trie tree implemented in ANSI C
 * Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

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
#include <stdlib.h>
#include <string.h>

// Definition
struct Trie{
  struct Trie* children[26];
  void* data;
};

struct Trie* create_node(){
  struct Trie* t = (struct Trie*)malloc(sizeof(struct Trie));
  int i;
  for(i=0; i<26; ++i)
    t->children[i]=0;
  t->data=0;
  return t;
}

void destroy(struct Trie* t){
  if(!t)
    return;

  int i;
  for(i=0; i<26; ++i)
    destroy(t->children[i]);

  if(t->data)
    free(t->data);
  free(t);
}

struct Trie* insert(struct Trie* t, const char* key, void* value){
  if(!t)
    t=create_node();

  struct Trie* p =t;
  while(*key){
    int c = *key - 'a';
    if(!p->children[c])
      p->children[c] = create_node();
    p = p->children[c];
    ++key;
  }
  p->data = value;
  return t;
}

// test helpers
void print_trie(struct Trie* t, const char* prefix){
  printf("(%s", prefix);
  if(t->data)
    printf(":%s", (char*)(t->data));
  int i;
  for(i=0; i<26; ++i){
    if(t->children[i]){
      printf(", ");
      char* new_prefix=(char*)malloc(strlen(prefix+1)*sizeof(char));
      sprintf(new_prefix, "%s%c", prefix, i+'a');
      print_trie(t->children[i], new_prefix);
    }
  }
  printf(")");
}

struct Trie* test_insert(){
  struct Trie* t=0;
  t = insert(t, "a", 0);
  t = insert(t, "an", 0);
  t = insert(t, "another", 0);
  t = insert(t, "boy", 0);
  t = insert(t, "bool", 0);
  t = insert(t, "zoo", 0);
  print_trie(t, "");
  return t;
}

int main(int argc, char** argv){
  struct Trie* t = test_insert();
  destroy(t);
  return 0;
}

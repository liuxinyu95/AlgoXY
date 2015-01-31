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

/* Definition */
struct Trie {
    struct Trie* children[26];
    void* data;
};

struct Trie* create_node() {
    struct Trie* t = (struct Trie*) malloc(sizeof(struct Trie));
    int i;
    for (i=0; i<26; ++i)
        t->children[i] = NULL;
    t->data = NULL;
    return t;
}

void destroy(struct Trie* t) {
    int i;
    if(t) {
        for (i=0; i<26; ++i)
            destroy(t->children[i]);
        if (t->data)
            free(t->data);
        free(t);
    }
}

struct Trie* insert(struct Trie* t, const char* key, void* value) {
    int c;
    struct Trie *p;
    if(!t)
        t=create_node();
    for (p = t; *key; ++key, p = p->children[c]) {
        c = *key - 'a';
        if (!p->children[c])
            p->children[c] = create_node();
    }
    p->data = value;
    return t;
}

void* lookup(struct Trie* t, const char* key) {
    while (*key && t && t->children[*key - 'a'])
        t = t->children[*key++ - 'a'];
    return (*key || !t) ? NULL : t->data;
}

// test helpers
void print_trie(struct Trie* t, const char* prefix) {
    printf("(%s", prefix);
    if(t->data)
        printf(":%s", (char*)(t->data));
    int i;
    for (i=0; i<26; ++i) {
        if(t->children[i]) {
            printf(", ");
            char* new_prefix=(char*)malloc(strlen(prefix+1)*sizeof(char));
            sprintf(new_prefix, "%s%c", prefix, i+'a');
            print_trie(t->children[i], new_prefix);
        }
    }
    printf(")");
}

struct Trie* test_insert() {
    struct Trie* t=0;
    t = insert(t, "a", strdup("A"));
    t = insert(t, "an", strdup("AN"));
    t = insert(t, "another", strdup("ANOTHER"));
    t = insert(t, "boy", strdup("BOY"));
    t = insert(t, "bool", strdup("BOOL"));
    t = insert(t, "zoo", strdup("ZOO"));
    print_trie(t, "");
    printf("\n");
    return t;
}

void test_lookup(struct Trie* t) {
    void* v = lookup(t, "another");
    printf("lookup 'another' ==> %s\n", v ? (char*)v : "not found");
    v = lookup(t, "boolean");
    printf("lookup 'boolean' ==> %s\n", v ? (char*)v : "not found");
}

int main(int argc, char** argv) {
    struct Trie* t = test_insert();
    test_lookup(t);
    destroy(t);
    return 0;
}

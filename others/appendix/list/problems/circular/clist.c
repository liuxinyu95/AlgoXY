/*
 * clist.c
 * Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

/*Circular linked-list problem is a speical Cycle detection problem*/

#define N 100

struct Node {
    int key;
    struct Node* next;
};

/*Detect if a linked-list is circular*/

/* Method 1, Robert W. Floyd algorithm, AKA tortoise and hare algorithm. */
int is_circular(struct Node* h) {
    struct Node *a, *b;
    a = b = h;
    while (a && b) {
        a = a->next;
        b = b->next;
        if (!b)
            break;
        b = b->next;
        if (a == b) return 1;
    }
    return 0;
}

/* Method 2, reverse list
 * After reversing, a normal linked-list exchange its head and tail;
 * while a reversed circular linked-list, its new head is as same as its original head.
 */

struct Node* reverse(struct Node* h) {
    struct Node *p = h, *h1 = NULL;
    while (h) {
        h = p->next;
        p->next = h1;
        h1 = p;
        p = h;
    }
    return h1;
}

int detect(struct Node* h) {
    struct Node* h1 = reverse(h);
    reverse(h1); /*resume the original list*/
    return h == h1;
}

/*Locate where the loop starts*/

/* Method 1, Robert W. Floyd tortoise and hare algorithm. */
struct Node* find_loop(struct Node* h) {
    struct Node *a, *b;
    a = b = h;
    while (a && b) {
        a = a->next;
        b = b->next;
        if (!b) break;
        b = b->next;
        if (a == b) {
            for (b = h; b != a; a = a->next, b = b->next);
            return a;
        }
    }
    return NULL; /*no loop*/
}

/* Method 2, Richard P. Brent's algorithm
 * Returns NULL if no cycle, otherwise, returns the connection point
*/
struct Node* find_cycle(struct Node* h) {
    struct Node *a, *b;
    int n = 0, power = 1;
    a = b = h;
    do {
        if (n == power) {
            a = b;
            power <<= 1;
            n = 0;
        }
        if (!b) break;
        b = b->next;
        ++n;
    } while (a && b && a != b);

    if (!a || !b) return NULL;

    for (b = h; n; --n, b = b->next);
    for (a = h; a != b; a = a->next, b = b->next);
    return a; /*or b*/
}

struct Node* create(int n, int k, int circular) {
    struct Node *p, *t = NULL, *h = NULL;
    while(n--) {
        p = (struct Node*) malloc(sizeof(struct Node));
        p -> key = rand() % 100;
        p -> next = h;
        h = p;
        if (!t) t = p;
    }
    if (circular) {
        while(k--) p = p->next;
        t->next = p;
    }
    return h;
}

void release(struct Node* h, int n) {
    struct Node* p;
    while (n--) {
        p = h;
        h = h->next;
        free(p);
    }
}

struct Node* get_at(struct Node* h, int k) {
    while(h && k--)
        h = h->next;
    return h;
}

void test() {
    int i = N, n, k, circular;
    while (i--) {
        n = rand() % 1000;
        k = n? rand() % n : 0;
        circular = rand() % 2;
        struct Node* h = create(n, k, circular);
        assert(is_circular(h) == circular);
        assert(detect(h) == circular);
        assert(find_loop(h) == (circular? get_at(h, k) : NULL));
        assert(find_cycle(h) == (circular? get_at(h, k) : NULL));
        release(h, n);
    }
    printf("%d test cases passed\n", N);
}

int main(int argc, char** argv){
    test();
    return 0;
}

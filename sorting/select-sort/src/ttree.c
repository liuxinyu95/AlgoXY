/*
 * ttree.c
 * Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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
/*
 * Tournament tree based selection sort
 * [1] Donald E. Knuth. ``The Art of Computer Programming, Volume 3: Sorting and Searching (2nd Edition)''. 
 * Addison-Wesley Professional; 2 edition (May 4, 1998) ISBN-10: 0201896850 ISBN-13: 978-0201896855
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N_INF -65535

typedef int Key;

Key max(Key x, Key y) {
    return x < y ? y : x;
}

struct Node {
    Key key;
    struct Node *left, *right, *parent;
};

int isleaf(struct Node* t) {
    return t && !t->left && !t->right;
}

struct Node* leaf(Key x) {
    struct Node* t = (struct Node*) malloc(sizeof(struct Node));
    t->key = x;
    t->left = t->right = t->parent = NULL;
    return t;
}

struct Node* branch(Key x, struct Node* l, struct Node* r) {
    struct Node* t = leaf(x);
    t->left = l;
    t->right = r;
    if (l)
        l->parent = t;
    if (r)
        r->parent = t;
    return t;
}

void release(struct Node* t) {
    if (t) {
        release(t->left);
        release(t->right);
        free(t);
    }
}

void prtree(struct Node* t) {
    if (t) {
        printf("(");
        prtree(t->left);
        printf(" %d ", t->key);
        prtree(t->right);
        printf(")");
    }
    else
        printf(".");
}

void print(struct Node* t) {
    prtree(t);
    printf("\n");
}

/* limitation: n = 2^m for some m in N */
struct Node* build(const Key* xs, int n) {
    int i;
    struct Node *t, **ts = (struct Node**) malloc(sizeof(struct Node*) * n);
    for (i = 0; i < n; ++i)
        ts[i] = leaf(xs[i]);
    for (; n > 1; n /= 2)
        for (i = 0; i < n; i += 2)
            ts[i/2] = branch(max(ts[i]->key, ts[i+1]->key), ts[i], ts[i+1]);
    t = ts[0];
    free(ts);
    return t;
}

/* limitation: for all x, x > -inf */
Key pop(struct Node* t) {
    Key x = t->key;
    t->key = N_INF;
    while (!isleaf(t)) {
        t = t->left->key == x ? t->left : t->right;
        t->key = N_INF;
    }
    while (t->parent) {
        t = t->parent;
        t->key = max(t->left->key, t->right->key);
    }
    return x;
}

/*testing*/

void tsort(Key* xs, int n) {
    struct Node* t = build(xs, n);
    while(n)
        xs[--n] = pop(t);
    release(t);
}

int main() {
    int i, ys[16];
    const int xs[] = {7, 6, 15, 16, 8, 4, 13, 3, 5, 10, 9, 1, 12, 2, 11, 14};
    struct Node* t = build(xs, sizeof(xs)/sizeof(xs[0]));
    print(t);
    release(t);
    memcpy((void*)ys, (void*)xs, sizeof(xs));
    tsort(ys, 16);
    for (i = 0; i < 16; ++i)
        printf(i == 15 ? "%d\n" : "%d, ", ys[i]);
    return 0;
}

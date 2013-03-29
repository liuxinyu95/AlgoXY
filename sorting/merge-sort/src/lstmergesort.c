/*
 * lstmergesort.c
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <time.h> /*for time measurement*/

typedef int Key;

#define N  100000

#define swap(a, b) {struct Node *tmp = a; a = b; b = tmp;}



/*
 * A linked-list merge sort
 * Refer to TAOCP:
 * Donald E. Knuth. ``The Art of Computer Programming, Volume 3: Sorting and Searching (2nd Edition)''. 
 *    Addison-Wesley Professional; 2 edition (May 4, 1998) ISBN-10: 0201896850 ISBN-13: 978-0201896855
 */

struct Node {
    Key key;
    struct Node* next;
};

struct Node* cons(Key x, struct Node* xs) {
    struct Node *p = (struct Node*) malloc(sizeof(struct Node));
    p->key = x;
    p->next = xs;
    return p;
}

struct Node* link(struct Node* xs, struct Node* ys) {
    xs->next = ys;
    return xs;
}

void release(struct Node* xs) {
    struct Node* p;
    while (xs) {
        p = xs;
        xs = xs->next;
        free(p);
    }
}

struct Node* to_list(const Key* xs, int n) {
    struct Node* ys = NULL;
    while (n--)
        ys = cons(xs[n], ys);
    return ys;
}

void from_list(Key* xs, struct Node* ys) {
    while(ys) {
        *xs++ = ys->key;
        ys = ys->next;
    }
}

struct Node* merge(struct Node* as, struct Node* bs) {
    struct Node s, *p;
    p = &s;
    while (as && bs) {
        if (as->key < bs->key) {
            link(p, as);
            as = as->next;
        }
        else {
            link(p, bs);
            bs = bs->next;
        }
        p = p->next;
    }
    if (as)
        link(p, as);
    if (bs)
        link(p, bs);
    return s.next;
}

struct Node* msort(struct Node* xs) {
    struct Node *p, *as, *bs;
    if (!xs || !xs->next)
        return xs;

    as = bs = NULL;
    while(xs) {
        p = xs;
        xs = xs->next;
        as = link(p, as);
        swap(as, bs);
    }
    as = msort(as);
    bs = msort(bs);
    return merge(as, bs);
}

void listsort(Key* xs, int l, int u) {
    struct Node* lst = msort(to_list(xs + l, u - l));
    from_list(xs + l, lst);
    release(lst);
}


/* test */
int cmp(const void* x, const void* y) {
    return *(Key*)x - *(Key*)y;
}

void testmsort(void (*fsort)(Key*, int, int)) {
    int i, j, n, xs[N], ys[N];
    for (j = 0; j < 100; ++j) {
        for (i = 0, n = rand() % N; i < n; ++i)
            xs[i] = rand() % N;
        memcpy((void*)ys, (const void*)xs, n * sizeof(int));
        qsort(xs, n, sizeof(int), cmp);
        fsort(ys, 0, n);
        assert(!memcmp(xs, ys, n * sizeof(int)));
    }
}

int main() {
    double t = clock();
    testmsort(listsort);
    printf("list merge sort tested, average time: %f\n", (clock() - t) / CLOCKS_PER_SEC / 100.0);
    return 0;
}

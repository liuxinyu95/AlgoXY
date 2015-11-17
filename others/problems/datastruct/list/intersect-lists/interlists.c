#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef int Key;

struct Node {
    Key key;
    struct Node *next;
};

unsigned len(struct Node* xs) {
    unsigned n;
    for (n = 0; xs; xs = xs->next, ++n);
    return n;
}

/*
 * Given 2 lists L1, and L2, returns the first intersection node.
 * Returns NIL if L1 doesn't intersect to L2.
 *
 * Let len1 = |L1|, len2 = |L2|, suppose len1 >= len2 without loss of generality.
 * Advance L1 for len1 - len2 steps, then advance both L1 and L2 in parallel until
 * L1 meets L2 or both reach the ends.
 */
struct Node* intersect(struct Node* xs, struct Node* ys) {
    unsigned len1 = len(xs), len2 = len(ys);
    unsigned n = len1 < len2 ? len2 - len1 : len1 - len2;
    struct Node* tmp;
    if (xs == NULL || ys == NULL) return NULL;
    if (len1 < len2) {
        tmp = xs; xs = ys; ys = tmp;
    }
    while (n--)
        xs = xs->next;
    for (; xs && xs != ys; xs = xs->next, ys = ys->next);
    return xs;
}

/* The following is for verification purpose. */
struct Node* node(Key x) {
    struct Node* p = (struct Node*) malloc(sizeof(struct Node));
    p->key = x;
    p->next = NULL;
    return p;
}

struct Node* link(struct Node* x, struct Node* y) {
    x->next = y;
    return x;
}

void printList(struct Node* xs) {
    for (; xs; xs = xs->next)
        printf(xs->next ? "%d-->" : "%d\n", xs->key);
}

int main(int argc, char** argv) {
    int i;
    struct Node* ns[] = {node(0), node(1), node(2), node(3), node(4), node(5), node(6)};
    struct Node* xs = link(ns[0], link(ns[1], link(ns[2], link(ns[3], link(ns[4], NULL)))));
    struct Node* ys = link(ns[5], link(ns[6], ns[1]));
    struct Node* p = intersect(xs, ys);
    printList(xs);
    printList(ys);
    printf("intersect at %d\n", p->key);
    assert(p->key == 1);

    link(ns[6], NULL);
    p = intersect(xs, ys);
    printList(xs);
    printList(ys);
    printf("intersect at null");
    assert(p == NULL);

    for (i=0; i < sizeof(ns)/sizeof(ns[0]); ++i)
        free(ns[i]);
    return 0;
}

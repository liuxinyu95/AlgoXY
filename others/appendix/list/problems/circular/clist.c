#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#define N 100

struct Node {
    int key;
    struct Node* next;
};

/*Detect if a linked-list is circular*/
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
    while (--n) {
        p = h;
        h = h->next;
        free(p);
    }
}

void test() {
    int i = N, n, k, circular;
    while (i--) {
        n = rand() % 1000;
        k = n? rand() % n : 0;
        circular = rand() % 2;
        struct Node* h = create(n, k, circular);
        assert(is_circular(h) == circular);
        release(h, n);
    }
    printf("%d test cases passed\n", N);
}

int main(int argc, char** argv){
    test();
    return 0;
}

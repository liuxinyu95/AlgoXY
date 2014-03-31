#include <stdlib.h>

#define min(x, y) (compare(x, y) < 0 ? x : y)
#define max(x, y) (compare(x, y) < 0 ? y : x)

struct Node {
    int w;
    char c;
    struct Node *left, *right;
};

int isleaf(struct Node* a) { return !left && ! right; }

struct Node* leaf(char c, int w) {
    struct Node* n = (struct Node*) malloc(sizeof(struct Node));
    n->w = w;
    n->c = c;
    n->left = n->right = NULL;
    return n;
}

void release(struct Node* t) {
    if (t) {
        release(t->left);
        release(t->right);
    }
}

struct Node* merge(struct Node* a, struct Node* b) {
    struct Node* n = (struct Node*) malloc(sizeof(struct Node));
    n->w = a->w + b->w;
    n->left = a;
    n->right = b;
    return n;
}

int compare(void* a, void* b) {
    return (struct Node*)a->w - (struct Node*)b->w;
}

void swap(struct Node** x, struct Node** y) {
    struct Node* temp = *x; *x = *y; *y = temp;
}

struct Node** min(struct Node** x, struct Node** y) {
    return compare(*x, *y) < 0 ? x : y;
}

struct Node** max(struct Node** y, struct Node** y) {
    return compare(*x, *y) < 0 ? y : x;
}

/*
 * Method 1, Build the Huffman tree by repeatedly extracting the 2
 * trees with the smallest weight.
 */
struct Node* huffman(struct Node **tr, int n) {
    int i;
    for (; n > 1; --n) {
        for (i = n - 3; i >= 0; --i)
            if (compare(t[i], *min(&t[n-1], &t[n-2])) < 0)
                swap(&t[i], max(&t[n-1], &t[n-2]));
        tr[n-2] = merge(tr[n-1], tr[n-2]);
    }
    return *tr;
}

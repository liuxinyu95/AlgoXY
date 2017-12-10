#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

using namespace std;

typedef int Key;

struct Node;
Node* replace(Node* parent, Node* x, Node* y);

struct Node {
    Key key;
    int delta;
    Node *left, *right, *parent;
    Node(Key k) : key(k), delta(0), left(nullptr), right(nullptr), parent(nullptr) {}

    virtual ~Node() {
        delete left;
        delete right;
    }

    void setLeft(Node* x) {
        left = x;
        if (x) x->parent = this;
    }

    void setRight(Node* x) {
        right = x;
        if (x) x->parent = this;
    }

    void setChildren(Node* x, Node* y) {
        setLeft(x);
        setRight(y);
    }

    Node* replaceWith(Node* y) {
        return replace(parent, this, y);
    }
};

bool isLeaf(Node* x) {
    return x && x->left == nullptr && x->right == nullptr;
}

// change from: parent --> x to parent --> y
Node* replace(Node* parent, Node* x, Node* y) {
    if (!parent) {
        if (y) y->parent = nullptr;
    } else if (parent->left == x) {
        parent->setLeft(y);
    } else {
        parent->setRight(y);
    }
    if (x) x->parent = nullptr;
    return y;
}

Node* min(Node* t) {
    while (t && t->left) t = t->left;
    return t;
}

Node* search(Node* t, Key x) {
    while (t && t->key != x)
        t = x < t->key ? t->left : t->right;
    return t;
}

void remove(Node* x) {
    if (x) {
        x->parent = x->left = x->right = nullptr;
        delete x;
    }
}

// Rotation. It doens't change the delta

// left rotation: (a, x, (b, y, c)) ==> ((a, x, b), y, c)
Node* leftRotate(Node* t, Node* x) {
    Node* parent = x->parent;
    Node* y = x->right;
    Node* a = x->left;
    Node* b = y->left;
    Node* c = y->right;
    x->replaceWith(y);
    x->setChildren(a, b);
    y->setChildren(x, c);
    if (!parent) t = y;
    return t;
}

// right rotation: (a, x, (b, y, c)) <== ((a, x, b), y, c)
Node* rightRotate(Node* t, Node* y) {
    Node* parent = y->parent;
    Node* x = y->left;
    Node* a = x->left;
    Node* b = x->right;
    Node* c = y->right;
    y->replaceWith(x);
    y->setChildren(b, c);
    x->setChildren(a, y);
    if (!parent) t = x;
    return t;
}

// Insertion

Node* insertFix(Node* t, Node* parent, Node* x);

// top-down insert, returns the new root
Node* insert(Node* t, Key key) {
    Node* root = t;
    Node* x = new Node(key);
    Node* parent = nullptr;
    while (t) {
        parent = t;
        t = key < t->key ? t->left : t->right;
    }
    if (!parent) // tree is empty
        root = x;
    else if (key < parent->key)
        parent->setLeft(x);
    else
        parent->setRight(x);
    return insertFix(root, parent, x);
}

/*
 * Bottom-up update delta and fix
 *   t: tree root;
 *   x: the sub-tree that the height changes.
 *
 * Denote d = delta(x), d' = delta(x'),
 *   where x' is the new sub tree after insertion.
 *
 * case 1: |d| == 0, |d'| == 1,
 *    It means height increase, go on bottom-up updating.
 *
 * case 2: |d| == 1, |d'| == 0,
 *    program terminate as height doesn't change.
 *
 * case 3: |d| == 1, |d'| == 2, AVL violation,
 *    we need fixing by rotation.
 */
Node* insertFix(Node* t, Node* parent, Node* x) {
    int d1, d2, dy;
    Node *p, *l, *r;
    while (parent) {
        d2 = d1 = parent->delta;
        d2 += (x == parent->left ? -1 : 1);
        parent->delta = d2;
        p = parent;
        l = parent->left;
        r = parent->right;
        if (abs(d1) == 1 && abs(d2) == 0) {
            return t;
        } else if (abs(d1) == 0 && abs(d2) == 1) {
            x = parent;
            parent = x->parent;
        } else if (abs(d1) == 1 && abs(d2) == 2) {
            if (d2 == 2) {
                if (r->delta == 1) { // right-right case
                    p->delta = 0;
                    r->delta = 0;
                    t = leftRotate(t, p);
                } else if (r->delta == -1) { // right-left case
                    dy = r->left->delta;
                    p->delta = dy == 1 ? -1 : 0;
                    r->left->delta = 0;
                    r->delta = dy == -1 ? 1 : 0;
                    t = rightRotate(t, r);
                    t = leftRotate(t, p);
                }
            } else if (d2 == -2) {
                if (l->delta == -1) { // left-left case
                    p->delta = 0;
                    l->delta = 0;
                    t = rightRotate(t, p);
                } else if (l->delta == 1) { // left-right case
                    dy = l->right->delta;
                    l->delta = dy == 1 ? -1 : 0;
                    l->right->delta = 0;
                    p->delta = dy == -1 ? 1 : 0;
                    t = leftRotate(t, l);
                    t = rightRotate(t, p);
                }
            }
            break;
        } else {
            printf("shouldn't be here. d1=%d, d2=%d\n", d1, d2);
            assert(false);
        }
    }
    return t;
}

Node* deleteFix(Node* t, Node* parent, Node* x);

Node* del(Node* t, Node* x) {
    if (!x) return t;
    Node *y, *parent = x->parent;
    if (!x->left) {
        y = x->replaceWith(x->right);
    } else if (!x->right) {
        y = x->replaceWith(x->left);
    } else {
        y = min(x->right);
        x->key = y->key;
        parent = y->parent;
        x = y;
        y = y->replaceWith(y->right);
    }
    t = deleteFix(t, parent, y);
    remove(x);
    return t;
}

/*
 * Bottom-up update delta and fix
 *   t: tree root;
 *   x: the sub-tree that the height changes.
 *
 * Denote d = delta(x), d' = delta(x'),
 *   where x' is the new sub tree after deletion.
 *
 * case 1: |d| == 0, |d'| == 1,
 *    Program terminate as height doesn't change.
 *
 * case 2: |d| == 1, |d'| == 0,
 *    For delete, it means height decrease, go on bottom-up updating.
 *
 * case 3: |d| == 1, |d'| == 2, AVL violation,
 *    we need fixing by rotation.
 */
Node* deleteFix(Node* t, Node* parent, Node* x) {
    int d1, d2, dy;
    Node *p, *l, *r;
    while (parent) {
        d2 = d1 = parent->delta;
        d2 += (x == parent->left ? 1 : -1);
        if (isLeaf(parent)) d2 = 0;
        parent->delta = d2;
        p = parent;
        l = parent->left;
        r = parent->right;
        if (abs(d1) == 1 && abs(d2) == 0) {
            x = parent;
            parent = x->parent;
        } else if (abs(d1) == 0 && abs(d2) == 1) {
            return t;
        } else if (abs(d1) == 1 && abs(d2) == 2) {
            if (d2 == 2) {
                if (r->delta == 1) {  // right-right case
                    p->delta = r->delta = 0;
                    parent = r;
                    t = leftRotate(t, p);
                } else if (r->delta == -1) { // right-left case
                    dy = r->left->delta;
                    p->delta = dy == 1 ? -1 : 0;
                    r->left->delta = 0;
                    r->delta = dy == -1 ? 1 : 0;
                    parent = r->left;
                    t = rightRotate(t, r);
                    t = leftRotate(t, p);
                } else { // delete specific right-right case
                    p->delta = 1;
                    r->delta--;
                    t = leftRotate(t, p);
                    break; // no further height change
                }
            } else if (d2 == -2) {
                if (l->delta == -1) { // left-left case
                    p->delta = l->delta = 0;
                    parent = l;
                    t = rightRotate(t, p);
                } else if (l->delta == 1) { // left-right case
                    dy = l->right->delta;
                    l->delta = dy == 1 ? -1 : 0;
                    l->right->delta = 0;
                    p->delta = dy == -1 ? 1 : 0;
                    parent = l->right;
                    t = leftRotate(t, l);
                    t = rightRotate(t, p);
                } else { // delete specific left-left case
                    p->delta = -1;
                    l->delta++;
                    t = rightRotate(t, p);
                    break; // no further height change
                }
            }
            // the 4 rebalance cases cause height decrease, go on bottom-up update
            x = parent;
            parent = x->parent;
        } else {
            printf("shouldn't be here, d1 = %d, d2 = %d", d1, d2);
            assert(false);
        }
    }
    if (!parent) return x; // delete the root
    return t;
}

// helpers

int height(Node* t) {
    return t == nullptr ? 0 : (1 + max(height(t->left), height(t->right)));
}

vector<Key> toList(Node* t) {
    vector<Key> xs, ys;
    if (t) {
        xs = toList(t->left);
        xs.push_back(t->key);
        ys = toList(t->right);
        xs.insert(xs.end(), ys.begin(), ys.end());
    }
    return xs;
}

template<typename XS>
Node* fromList(XS xs) {
    Node* t = nullptr;
    for (auto x : xs)
        t = insert(t, x);
    return t;
}

string toStr(Node* t) {
    if (t == nullptr) return ".";
    ostringstream s;
    s << "(" << toStr(t->left) << " " << t->key << ":" << t->delta
      << " " << toStr(t->right) << ")";
    return s.str();
}

bool isAVL(Node* t) {
    if (t == nullptr) return true;
    int delta = height(t->right) - height(t->left);
    return delta == t->delta && isAVL(t->left) && isAVL(t->right) && abs(delta) <= 1;
}

bool isBST(Node* t, vector<Key> xs) {
    sort(xs.begin(), xs.end());
    return xs == toList(t);
}

const static int N = 100;
static vector<Key> num(N);

vector<Key> genList(int maxLen) {
    for (int i = 0; i < N; ++i)
        swap(num[i], num[rand() % N]);
    return vector<Key>(num.begin(), num.begin() + maxLen);
}

void testInsert(vector<Key>& xs) {
    Node* t = fromList(xs);
    if (!isBST(t, xs)) {
        copy(xs.begin(), xs.end(), ostream_iterator<Key>(cout, ", "));
        printf("\nbuild violated AVL properties: t = %s\n", toStr(t).c_str());
        assert(false);
    }
    delete t;
}

Node* testDeleteKey(Node* t, Key k) {
    t = del(t, search(t, k));
    if (search(t, k)) {
        printf("Found %d after delete.\n", k);
        assert(false);
    }
    if (!isAVL(t)) {
        printf("del violated AVL properties: t = %s\n", toStr(t).c_str());
        assert(false);
    }
    return t;
}

void testDelete(vector<Key>& xs) {
    Node* t = fromList(xs);
    for (vector<Key>::iterator it = xs.begin(); it != xs.end(); ++it)
        t = testDeleteKey(t, *it);
    delete t;
}

int main(int argc, char** argv) {
    int i;
    for (i = 0; i < N; ++i)
        num[i] = i;
    for (i = 0; i < N; ++i) {
        vector<Key> xs = genList(N);
        testInsert(xs);
        testDelete(xs);
    }
    printf("%d tests passed.\n", N);
    return 0;
}

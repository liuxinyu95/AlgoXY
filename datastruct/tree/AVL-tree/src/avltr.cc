#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>

using namespace std;

typedef int Key;

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

    // parent <--> this  ==> parent <--> y
    void replaceWith(Node* y) {
        if (!parent) {
            if (y) y->parent = nullptr;
        } else if (parent->left == this) {
            parent->setLeft(y);
        } else {
            parent->setRight(y);
        }
        parent = nullptr;
    }
};

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

Node* insertFix(Node* t, Node* x);

// top-down insert
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
    return insertFix(root, x);
}

/*
 * bottom-up update delta and fix
 *   t: tree root;
 *   x: the sub-tree that the height increases.
 */
Node* insertFix(Node* t, Node* x) {
    /*
     * denote d = delta(t), d' = delta(t'),
     *   where t' is the new tree after insertion.
     *
     * case 1: |d| == 0, |d'| == 1, height increase,
     *    we need go on bottom-up updating.
     *
     * case 2: |d| == 1, |d'| == 0, height doesn't change,
     *    program terminate
     *
     * case 3: |d| == 1, |d'| == 2, AVL violation,
     *    we need fixing by rotation.
     */
    int d1, d2, dy;
    Node *p, *l, *r;
    while (x->parent) {
        d2 = d1 = x->parent->delta;
        d2 += x == x->parent->left ? -1 : 1;
        x->parent->delta = d2;
        p = x->parent;
        l = x->parent->left;
        r = x->parent->right;
        if (abs(d1) == 1 && abs(d2) == 0) {
            return t;
        } else if (abs(d1) == 0 && abs(d2) == 1) {
            x = x->parent;
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

// helpers

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

int height(Node* t) {
    if (t == nullptr)
        return 0;
    else
        return 1 + max(height(t->left), height(t->right));
}

bool isAVL(Node* t) {
    if (t == nullptr)
        return true;
    else {
        int delta = height(t->right) - height(t->left);
        return isAVL(t->left) && isAVL(t->right) && abs(delta) <= 1;
    }
}

bool isBST(Node* t, vector<Key> xs) {
    sort(xs.begin(), xs.end());
    return xs == toList(t);
}

int main(int argc, char** argv) {
    int i, n, m = 1000;
    vector<Key> xs(1000), ys;
    Node* t;

    printf("test insert...\n");
    while (m--) {
        for (i = 0; i < 10; ++i)
            xs[i] = i;
        n = rand() % 1000;
        for (i = 0; i < n; ++i)
            swap(xs[i], xs[rand() % 1000]);
        ys = vector<Key>(xs.begin(), xs.begin() + rand() % 1000);
        t = fromList(ys);
        if (!isBST(t, ys)) {
            copy(ys.begin(), ys.end(), ostream_iterator<Key>(cout, ", "));
            printf("t=%s\n", toStr(t).c_str());
            assert(false);
        }
        if (!isAVL(t)) {
            printf("not AVL!\nt=%s\n", toStr(t).c_str());
            assert(false);
        }
        delete t;
    }
    printf("done\n");
    return 0;
}

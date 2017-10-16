/*
 * rbt.cc
 * Copyright (C) 2016 Liu Xinyu (liuxinyu95@gmail.com)
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
 *
 */
#include <cstdlib>
#include <string>
#include <vector>
#include <sstream>
#include <iostream>
#include <cassert>

using namespace std;

typedef int Key;

enum class Color { RED, BLACK, DOUBLY_BLACK };

struct Node {
    Key key;
    Color color;
    Node* left;
    Node* right;
    Node* parent;

    Node(Key k, Color c = Color::RED) : key(k), color(c),
                                        left(nullptr), right(nullptr),
                                        parent(nullptr) {}

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

    void replaceWith(Node* y) {
        replace(parent, this, y);
    }

    Node* sibling() {
        return parent->left == this ? parent->right : parent->left;
    }

    Node* uncle() {
        return parent->sibling();
    }

    Node* grandparent() {
        return parent->parent;
    }

    // change from: parent --> x to parent --> y
    static Node* replace(Node* parent, Node* x, Node* y) {
        if (parent == nullptr) {
            if (y) y->parent = nullptr;
        } else if (parent->left == x) {
            parent->setLeft(y);
        } else {
            parent->setRight(y);
        }
        if (x) x->parent = nullptr;
        return y;
    }

};

// helpers
void setColors(Node* x, Color a, Node* y, Color b) { x->color = a; y->color = b; }

void setColors(Node* x, Color a, Node* y, Color b, Node* z, Color c) {
    setColors(x, a, y, b);
    z->color = c;
}

void setColors(Node* x, Color a, Node* y, Color b,
               Node* z, Color c, Node* q, Color d) {
    setColors(x, a, y, b);
    setColors(z, c, q, d);
}

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

// insertion and deletion

Node* insertFix(Node* t, Node* x);

// returns the new root
Node* insert(Node* t, Key key) {
    Node* root = t;
    Node* x = new Node(key);
    Node* parent = nullptr;
    while (t) {
        parent = t;
        t = key < t->key ? t->left : t->right;
    }
    if (!parent) { //insert key to the empty tree
        root = x;
    } else if (key < parent -> key) {
        parent->setLeft(x);
    } else {
        parent->setRight(x);
    }
    return insertFix(root, x);
}

// fix the red->red violation
Node* insertFix(Node* t, Node* x) {
    while (x->parent && x->parent->color == Color::RED) {
        if (x->uncle()->color == Color::RED) {
            // case 1: ((a:R, x:R, b), y:B, c:R) ==> ((a:R, x:B, b), y:R, c:B)
            setColors(x->parent, Color::BLACK,
                      x->grandparent(), Color::RED,
                      x->uncle(), Color::BLACK);
            x = x->grandparent();
        } else {
            if (x->parent == x->grandparent()->left) {
                if (x == x->parent->right) {
                    // case 2: ((a, x:R, b:R), y:B, c) ==> case 3
                    x = x->parent;
                    t = leftRotate(t, x);
                }
                // case 3: ((a:R, x:R, b), y:B, c) ==> (a:R, x:B, (b, y:R, c))
                setColors(x->parent, Color::BLACK,
                          x->grandparent(), Color::RED);
                t = rightRotate(t, x->grandparent());
            } else {
                if (x == x->parent->left) {
                    // case 2': (a, x:B, (b:R, y:R, c)) ==> case 3'
                    x = x->parent;
                    t = rightRotate(t, x);
                }
                // case 3': (a, x:B, (b, y:R, c:R)) ==> ((a, x:R, b), y:B, c:R)
                setColors(x->parent, Color::BLACK,
                          x->grandparent(), Color::RED);
                t = leftRotate(t, x->grandparent());
            }
        }
    }
    t->color = Color::BLACK;
    return t;
}

Node* search(Node* t, Key x) {
    while (t && t->key != x)
        t = x < t->key ? t->left : t->right;
    return t;
}

Node* min(Node* t) {
    while (t && t->left) t = t->left;
    return t;
}

void remove(Node* x) {
    if (x) {
        x->parent = x->left = x->right = nullptr;
        delete x;
    }
}

bool isLeaf(Node* x) {
    return x != nullptr && (x->left == nullptr && x->right == nullptr);
}

bool isRed(Node* x) {
    return x != nullptr && x->color == Color::RED;
}

bool isBlack(Node* x) {
    return x == nullptr || x->color == Color::BLACK;
}

Node* blacken(Node* x) {
    x->color = isRed(x) ? Color::BLACK : Color::DOUBLY_BLACK;
    return x;
}

Node* makeBlack(Node* parent, Node* x) {
    if (!parent && ! x)
        return nullptr;
    if (!x)
        return Node::replace(parent, x, new Node(0, Color::DOUBLY_BLACK));
    return blacken(x);
}

Node* deleteFix(Node* t, Node* db, bool isDBEmpty);

Node* del(Node* t, Node* x) {
    if (!x) return t;
    Node* parent = x->parent;
    Node* db = nullptr;        //doubly black
    Node* y;

    if (x->left == nullptr) {
        db = x->right;
        x->replaceWith(db);
    } else if (x->right == nullptr) {
        db = x->left;
        x->replaceWith(db);
    } else {
        y = min(x->right);
        parent = y->parent;
        db = y->right;
        x->key = y->key;
        y->replaceWith(db);
        x = y;
    }
    if (x->color == Color::BLACK)
        t = deleteFix(t, makeBlack(parent, db), db == nullptr);
    remove(x);
    return t;
}

Node* deleteFix(Node* t, Node* db, bool isDBEmpty) {
    Node* dbEmpty = isDBEmpty ? db : nullptr;
    if (!db) return nullptr;    // remove the root from a leaf tree;
    while (db != t && db->color == Color::DOUBLY_BLACK) {
        if (db->sibling() != nullptr) {
            if (isRed(db->sibling())) {
                // case 1: the sibling is red, (transform to make the sibling black)
                setColors(db->parent, Color::RED,
                          db->sibling(), Color::BLACK);
                if (db == db->parent->left)
                    t = leftRotate(t, db->parent);
                else
                    t = rightRotate(t, db->parent);
            } else if (isBlack(db->sibling()) && isRed(db->sibling()->left)) {
                // case 3, 4: the sibling is black, and one nephew is red
                if (db == db->parent->left) {
                    setColors(db, Color::BLACK,
                              db->parent, Color::BLACK,
                              db->sibling()->left, db->parent->color);
                    t = rightRotate(t, db->sibling());
                    t = leftRotate(t, db->parent);
                } else {
                    setColors(db, Color::BLACK,
                              db->parent, Color::BLACK,
                              db->sibling(), db->parent->color,
                              db->sibling()->left, Color::BLACK);
                    t = rightRotate(t, db->parent);
                }
            } else if (isBlack(db->sibling()) && isRed(db->sibling()->right)) {
                if (db == db->parent->left) {
                    setColors(db, Color::BLACK,
                              db->parent, Color::BLACK,
                              db->sibling(), db->parent->color,
                              db->sibling()->right, Color::BLACK);
                    t = leftRotate(t, db->parent);
                } else {
                    setColors(db, Color::BLACK,
                              db->parent, Color::BLACK,
                              db->sibling()->right, db->parent->color);
                    t = leftRotate(t, db->sibling());
                    t = rightRotate(t, db->parent);
                }
            } else if (isBlack(db->sibling()) &&
                       isBlack(db->sibling()->left) &&
                       isBlack(db->sibling()->right)) {
                // case 2: the sibling and both nephews are black.
                //         move the blackness up
                setColors(db, Color::BLACK,
                          db->sibling(), Color::RED);
                blacken(db->parent);
                db = db->parent;
            }
        } else { // no sibling, we can move the blackness up
            db->color = Color::BLACK;
            blacken(db->parent);
            db = db->parent;
        }
    }
    t->color = Color::BLACK;
    if (dbEmpty) {
        dbEmpty->replaceWith(nullptr);
        delete dbEmpty;
    }
    return t;
}

// verification

Node* clone(Node* t) {
    Node* p = nullptr;
    if (t) {
        p = new Node(t->key, t->color);
        p->setChildren(clone(t->left), clone(t->right));
    }
    return p;
}

string toStr(Node* t) {
    if (t == nullptr) return ".";
    ostringstream s;
    s << "(" << toStr(t->left) << " " << t->key << ":"
      << (t->color == Color::RED ? "R" : "B") << " " << toStr(t->right) << ")";
    return s.str();
}

template<typename XS>
Node* fromList(XS xs) {
    Node* t = nullptr;
    for (auto x : xs)
        t = insert(t, x);
    return t;
}

struct Test {
    Node *t1, *t2;

    static Color colorOf(char c) {
        switch (c) {
        case 'R':
            return Color::RED;
        case 'B':
            return Color::BLACK;
        default:
            return Color::DOUBLY_BLACK;
        }
    }

    static Node* nodeOf(Key x, char c) {
        return new Node(x, colorOf(c));
    }

    static Node* tr(Node* l, Key x, char c, Node* r) {
        Node* t = nodeOf(x, c);
        t->setChildren(l, r);
        return t;
    }

    static bool isRBTree(Node* t) {
        if (t == nullptr) return true;
        if (!isBlack(t)) {
            printf("root is not black\n");
            return false;
        }
        if (hasAdjacentRed(t)) {
            printf("has adjacent red nodes\n");
            return false;
        }
        if (numOfBlacks(t) < 0) {
            printf("different number of black nodes\n");
            return false;
        }
        return true;
    }

    static bool hasAdjacentRed(Node* t) {
        if (t == nullptr) return false;
        if (isRed(t) && (isRed(t->left) || isRed(t->right))) {
            printf("adjacent red at %d\n", t->key);
            return true;
        }
        return hasAdjacentRed(t->left) || hasAdjacentRed(t->right);
    }

    static int numOfBlacks(Node* t) {
        if (t == nullptr) return 1;
        int a = numOfBlacks(t->left), b = numOfBlacks(t->right);
        if (a != b) {
            printf("Node %d has different black desendants: l=%d, r=%d\n", t->key, a, b);
            return -1000;
        }
        return a + (isBlack(t) ? 1 : 0);
    }

    template<typename Str, typename T>
    static void assertEqual(Str msg, T x, T y) {
        if (x == y)
            printf("%s OK\n", msg);
        else
            cout << msg << x << "!=" << y << " Fail\n";
    }

    static void assertEq(Node* a, Node* b) {
        string s1 = toStr(a), s2 = toStr(b);
        assertEqual("Different trees", s1, s2);
    }

    static void assertRBTree(Node* t) {
        assert(isRBTree(t));
    }

    Test() {
        // t1 = ((1:B, 2:R, (4:B, 3:R, .)), 5:B, (6:B, 7:R, (8:R, 9:B, .)))
        t1 = tr(tr(nodeOf(1, 'B'), 2, 'R', tr(nodeOf(3, 'R'), 4, 'B', nullptr)),
                   5, 'B',
                   tr(nodeOf(6, 'B'), 7, 'R', tr(nodeOf(8, 'R'), 9, 'B', nullptr)));
        printf("t1 1..9\n%s\n", toStr(t1).c_str());

        /*
         * t2 as figure 13.4 in CLRS
         * (((. 1:B .) 2:R ((. 5:R .) 7:B (. 8:R .))) 11:B (. 14:B (. 15:R .)))
         */
        t2 = tr(tr(nodeOf(1, 'B'), 2, 'R', tr(nodeOf(5, 'R'), 7, 'B', nodeOf(8, 'R'))),
                11, 'B',
                tr(nullptr, 14, 'B', nodeOf(15, 'R')));
        printf("t2, CLRS fig 13.4\n%s\n", toStr(t2).c_str());
    }

    ~Test() {
        delete t1;
        delete t2;
    }

    void run() {
        testRotate();
        testInsert();
        testDelete();
    }

    void testRotate() {
        Node* t = clone(t1);
        Node* x = t->right;    // 7:R
        t = leftRotate(t, x);  // (6 7 (8 9 .)) ==> ((6 7 8) 9 .)
        printf("left rotate at 7:R\n%s\n", toStr(t).c_str());
        t = rightRotate(t, t->right);  // rotate back
        printf("right rotate back:\n%s\n", toStr(t).c_str());
        assertEq(t, t1);

        t = leftRotate(t, t);  // (2 5 (6 7 9)) ==> ((2 5 6) 7 9)
        printf("left rotate at root:\n%s\n", toStr(t).c_str());
        t = rightRotate(t, t); // rotate back
        printf("right rotate back:\n%s\n", toStr(t).c_str());
        assertEq(t, t1);
        delete t;
    }

    void testInsert() {
        Node* t = clone(t2);
        t = insert(t, 4);
        printf("t2: after insert 4:\n%s\n", toStr(t).c_str());
        assertRBTree(t);
        delete t;

        t = fromList(vector<int>({5, 2, 7, 1, 4, 6, 9, 3, 8}));
        printf("list->tree, create t1 by insertion\n%s\n", toStr(t).c_str());
        assertEq(t, t1);
        assertRBTree(t);
        delete t;
    }

    void testDelete(Node* tree, int n) {
        Node* t = clone(tree);
        t = del(t, search(t, n));
        printf("del %d: %s\n", n, toStr(t).c_str());
        assertEqual("search after del: ", search(t, n), (Node*)nullptr);
        assertRBTree(t);
        delete t;
    }

    void testDelete() {
        int i;
        for (i = 1; i < 10; ++i)
            testDelete(t1, i);
        testDelete(t1, 11);     // del a non-exist value
        Node* t = new Node(1, Color::BLACK);    // leaf case
        testDelete(t, 1);
        delete t;
    }
};

int main(int, char**) {
    Test().run();
    return 0;
}

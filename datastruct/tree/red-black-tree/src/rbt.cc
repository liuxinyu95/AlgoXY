#include <cstdlib>
#include <string>
#include <sstream>

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
        if (left) delete left;
        if (right) delete right;
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

    Node* sibling() {
        return parent->left == this ? parent->right : parent->left;
    }

    Node* uncle() {
        return parent->sibling();
    }

    Node* grandparent() {
        return parent->parent;
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
    return x != nullptr && x->color == Color::BLACK;
}

void blacken(Node* x) {
    x->color = isRed(x) ? Color::BLACK : Color::DOUBLY_BLACK;
}

Node* makeBlack(Node* parent, Node* x) {
    if (!x) {
        if (isLeaf(parent))
            parent->color = Color::DOUBLY_BLACK;
        return parent;
    } else {
        blacken(x);
        return x;
    }
}

Node* deleteFix(Node* t, Node* db);

Node* del(Node* t, Node* x) {
    if (!x) return t;
    Node* parent = x->parent;
    Node* db = nullptr;        //doubly black
    Node* y;

    if (x->left == nullptr) {
        x->replaceWith(x->right);
        db = x->right;
    } else if (x->right == nullptr) {
        x->replaceWith(x->left);
        db = x->left;
    } else {
        y = min(x->right);
        parent = y->parent;
        db = y->right;
        x->key = y->key;
        y->replaceWith(y->right);
        x = y;
    }
    if (x->color == Color::BLACK)
        t = deleteFix(t, makeBlack(parent, db));
    remove(x);
    return t;
}

Node* deleteFix(Node* t, Node* db) {
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
                       !isRed(db->sibling()->left) &&
                       !isRed(db->sibling()->right)) {
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
    Test() {
        // t1 = ((1:B, 2:R, (4:B, 3:R, .)), 5:B, (6:B, 7:R, (8:R, 9:B, .)))
        t1 = new Node(5, Color::BLACK);
        t1->setChildren(new Node(2), new Node(7));
        t1->left->setChildren(new Node(1, Color::BLACK), new Node(4, Color::BLACK));
        t1->right->setChildren(new Node(6, Color::BLACK), new Node(9, Color::BLACK));
        t1->left->right->setLeft(new Node(3));
        t1->right->right->setLeft(new Node(8));
        printf("t1 1..9\n%s\n", toStr(t1).c_str());

        // t2 as figure 13.4 in CLRS
        t2 = new Node(11, Color::BLACK);
        t2->setChildren(new Node(2), new Node(14, Color::BLACK));
        t2->left->setChildren(new Node(1, Color::BLACK), new Node(7, Color::BLACK));
        t2->right->setRight(new Node(15));
        t2->left->right->setChildren(new Node(5), new Node(8));
        printf("t2, CLRS fig 13.4\n%s\n", toStr(t2).c_str());
    }

    ~Test() {
        delete t1;
        delete t2;
    }

    void run() {
        testRotate();
    }

    void testRotate() {
        Node* t = clone(t1);
        Node* x = t->right;    //7:R
        t = leftRotate(t, x);  // (6 7 (8 9 .)) ==> ((6 7 8) 9 .)
        printf("left rotate at 7:R\n%s\n", toStr(t).c_str());
        delete t;
    }
};

int main(int, char**) {
    Test t;
    t.run();
    return 0;
}

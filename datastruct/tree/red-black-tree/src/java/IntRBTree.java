/*
 * IntRBTree.java
 * Copyright (C) 2018 Liu Xinyu (liuxinyu95@gmail.com)
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
import java.util.*;

/*Red-black tree, with integral key*/

public class IntRBTree {

    public static enum Color { RED, BLACK, DOUBLY_BLACK }

    public static class Node {
        public int key;
        public Color color;
        public Node left;
        public Node right;
        public Node parent;

        public Node(int x, Color c) {
            key = x;
            color = c;
        }

        public Node(int x) { this(x, Color.RED); }

        void setLeft(Node x) {
            left = x;
            if (x != null) x.parent = this;
        }

        void setRight(Node x) {
            right = x;
            if (x != null) x.parent = this;
        }

        void setChildren(Node x, Node y) {
            setLeft(x);
            setRight(y);
        }

        void replaceWith(Node y) {
            replace(parent, this, y);
        }

        Node sibling() {
            return parent.left == this ? parent.right : parent.left;
        }

        Node uncle() { return parent.sibling(); }

        Node grandparent() { return parent.parent; }
    }

    // change from: parent --> x to parent --> y
    public static Node replace(Node parent, Node x, Node y) {
        if (parent == null) {
            if (y != null) y.parent = null;
        } else if (parent.left == x) {
            parent.setLeft(y);
        } else {
            parent.setRight(y);
        }
        if (x != null) x.parent = null;
        return y;
    }

    // auxiliary
    public static void setColors(Node x, Color a, Node y, Color b,
                                 Node z, Color c, Node q, Color d) {
        setColors(x, a, y, b);
        setColors(z, c, q, d);
    }

    public static void setColors(Node x, Color a, Node y, Color b, Node z, Color c) {
        setColors(x, a, y, b);
        z.color = c;
    }

    public static void setColors(Node x, Color a, Node y, Color b) {
        x.color = a;
        y.color = b;
    }

    // rotate left (a, x, (b, y, c)) ==> ((a, x, b), y, c)
    public static Node rotateLeft(Node t, Node x) {
        Node parent = x.parent;
        Node y = x.right;
        Node a = x.left;
        Node b = y.left;
        Node c = y.right;
        x.replaceWith(y);
        x.setChildren(a, b);
        y.setChildren(x, c);
        if (parent == null) t = y;
        return t;
    }

    // rotate right: (a, x, (b, y, c) <== ((a, x, b), y, c)
    public static Node rotateRight(Node t, Node y) {
        Node parent = y.parent;
        Node x = y.left;
        Node a = x.left;
        Node b = x.right;
        Node c = y.right;
        y.replaceWith(x);
        y.setChildren(b, c);
        x.setChildren(a, y);
        if (parent == null) t = x;
        return t;
    }

    // insertion and deletion

    // returns the new root
    public static Node insert(Node t, int key) {
        Node root = t;
        Node x = new Node(key);
        Node parent = null;
        while (t != null) {
            parent = t;
            t = key < t.key ? t.left : t.right;
        }
        if (parent == null) { // insert key to the empty tree
            root = x;
        } else if (key < parent.key) {
            parent.setLeft(x);
        } else {
            parent.setRight(x);
        }
        return insertFix(root, x);
    }

    // fix the red->red violation
    private static Node insertFix(Node t, Node x) {
        while (x.parent != null && x.parent.color == Color.RED) {
            if (x.uncle().color == Color.RED) {
                // case 1: ((a:R, x:R, b), y:B, c:R) ==> ((a:R, x:B, b), y:R, c:B)
                setColors(x.parent, Color.BLACK,
                          x.grandparent(), Color.RED,
                          x.uncle(), Color.BLACK);
                x = x.grandparent();
            } else {
                if (x.parent == x.grandparent().left) {
                    if (x == x.parent.right) {
                        // case 2: ((a, x:R, b:R), y:B, c) ==> case 3
                        x = x.parent;
                        t = rotateLeft(t, x);
                    }
                    // case 3: ((a:R, x:R, b), y:B, c) ==> (a:R, x:B, (b, y:R, c))
                    setColors(x.parent, Color.BLACK,
                              x.grandparent(), Color.RED);
                    t = rotateRight(t, x.grandparent());
                } else {
                    if (x == x.parent.left) {
                        // case 2': (a, x:B, (b:R, y:R, c)) ==> case 3'
                        x = x.parent;
                        t = rotateRight(t, x);
                    }
                    // case 3': (a, x:B, (b, y:R, c:R)) ==> ((a, x:R, b), y:B, c:R)
                    setColors(x.parent, Color.BLACK,
                              x.grandparent(), Color.RED);
                    t = rotateLeft(t, x.grandparent());
                }
            }
        }
        t.color = Color.BLACK;
        return t;
    }

    public static Node search(Node t, int x) {
        while (t != null && t.key != x)
            t = x < t.key ? t.left : t.right;
        return t;
    }

    public static Node min(Node t) {
        while (t != null && t.left != null) t = t.left;
        return t;
    }

    public static void remove(Node x) {
        if (x != null)
            x.parent = x.left = x.right = null;
    }

    public static boolean isLeaf(Node x) {
        return x != null && x.left == null && x.right == null;
    }

    public static boolean isRed(Node x) {
        return x != null && x.color == Color.RED;
    }

    public static boolean isBlack(Node x) {
        return x == null || x.color == Color.BLACK;
    }

    private static Node blacken(Node x) {
        x.color = isRed(x) ? Color.BLACK : Color.DOUBLY_BLACK;
        return x;
    }

    private static Node makeBlack(Node parent, Node x) {
        if (parent == null && x == null)
            return null;
        if (x == null)
            return replace(parent, x, new Node(0, Color.DOUBLY_BLACK));
        return blacken(x);
    }

    public static Node del(Node t, Node x) {
        if (x == null) return t;
        Node parent = x.parent;
        Node db = null;  // doubly black
        Node y;

        if (x.left == null) {
            db = x.right;
            x.replaceWith(db);
        } else if (x.right == null) {
            db = x.left;
            x.replaceWith(db);
        } else {
            y = min(x.right);
            parent = y.parent;
            db = y.right;
            x.key = y.key;
            y.replaceWith(db);
            x = y;
        }
        if (x.color == Color.BLACK)
            t = deleteFix(t, makeBlack(parent, db), db == null);
        remove(x);
        return t;
    }

    private static Node deleteFix(Node t, Node db, boolean isDBEmpty) {
        Node dbEmpty = isDBEmpty ? db : null;
        if (db == null) return null;   // remove the root from a leaf tree;
        while (db != t && db.color == Color.DOUBLY_BLACK) {
            if (db.sibling() != null) {
                if (isRed(db.sibling())) {
                    // case 1: the sibling is red, (transform to make the sibling black)
                    setColors(db.parent, Color.RED,
                              db.sibling(), Color.BLACK);
                    if (db == db.parent.left)
                        t = rotateLeft(t, db.parent);
                    else
                        t = rotateRight(t, db.parent);
                } else if (isBlack(db.sibling()) && isRed(db.sibling().left)) {
                    // case 3, 4: the sibling is black, and one nephew is red
                    if (db == db.parent.left) {
                        setColors(db, Color.BLACK,
                                  db.parent, Color.BLACK,
                                  db.sibling().left, db.parent.color);
                        t = rotateRight(t, db.sibling());
                        t = rotateLeft(t, db.parent);
                    } else {
                        setColors(db, Color.BLACK,
                                  db.parent, Color.BLACK,
                                  db.sibling(), db.parent.color,
                                  db.sibling().left, Color.BLACK);
                        t = rotateRight(t, db.parent);
                    }
                } else if (isBlack(db.sibling()) && isRed(db.sibling().right)) {
                    if (db == db.parent.left) {
                        setColors(db, Color.BLACK,
                                  db.parent, Color.BLACK,
                                  db.sibling(), db.parent.color,
                                  db.sibling().right, Color.BLACK);
                        t = rotateLeft(t, db.parent);
                    } else {
                        setColors(db, Color.BLACK,
                                  db.parent, Color.BLACK,
                                  db.sibling().right, db.parent.color);
                        t = rotateLeft(t, db.sibling());
                        t = rotateRight(t, db.parent);
                    }
                } else if (isBlack(db.sibling()) &&
                           isBlack(db.sibling().left) &&
                           isBlack(db.sibling().right)) {
                    // case 2: the sibling and both nephews are black.
                    //         move the blackness up
                    setColors(db, Color.BLACK,
                              db.sibling(), Color.RED);
                    blacken(db.parent);
                    db = db.parent;
                }
            } else { // no sibling, we can move the blackness up
                db.color = Color.BLACK;
                blacken(db.parent);
                db = db.parent;
            }
        }
        t.color = Color.BLACK;
        if (dbEmpty != null)
            dbEmpty.replaceWith(null);
        return t;
    }

    // verification

    public static Node clone(Node t) {
        Node p = null;
        if (t != null) {
            p = new Node(t.key, t.color);
            p.setChildren(clone(t.left), clone(t.right));
        }
        return p;
    }

    public static String toStr(Node t) {
        if (t == null) return ".";
        return String.format("(%s %d:%s %s)", toStr(t.left),
                             t.key, (t.color == Color.RED ? "R" : "B"),
                             toStr(t.right));
    }

    public static Node fromList(List<Integer> xs) {
        Node t = null;
        for (Integer x : xs)
            t = insert(t, x);
        return t;
    }
}

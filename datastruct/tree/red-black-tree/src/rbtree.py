#!/usr/bin/python

# rbtree.py
# Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import string

RED = 0
BLACK = 1
DOUBLY_BLACK = 2

class Node:
    def __init__(self, key, color = RED):
        self.key = key;
        self.color = color;
        self.left = self.right = self.parent = None

    def set_left(self, x):
        self.left = x
        if x != None:
            x.parent = self

    def set_right(self, x):
        self.right = x
        if x != None:
            x.parent = self

    def set_children(self, x, y):
        self.set_left(x)
        self.set_right(y)

    #parent<->self ==> parent<->y
    def replace_by(self, y):
        if self.parent is None:
            if y!= None: y.parent = None
        elif self.parent.left == self:
            self.parent.set_left(y)
        else:
            self.parent.set_right(y)
        self.parent = None

    def sibling(self):
        if self.parent.left == self:
            return self.parent.right
        else:
            return self.parent.left

    def uncle(self):
        return self.parent.sibling()

    def grandparent(self):
        return self.parent.parent

# common helpfer operations
def set_color(nodes, colors):
    for (n, c) in zip(nodes, colors):
        n.color = c

# rotations

# (a x (b y c)) ==> ((a x b) y c)
def left_rotate(t, x):
    (parent, y) = (x.parent, x.right)
    (a, b, c)   = (x.left, y.left, y.right)
    x.replace_by(y)
    x.set_children(a, b)
    y.set_children(x, c)
    if parent is None:
        t=y
    return t

# (a x (b y c)) <== ((a x b) y c)
def right_rotate(t, y):
    (parent, x) = (y.parent, y.left)
    (a, b, c)   = (x.left, x.right, y.right)
    y.replace_by(x)
    y.set_children(b, c)
    x.set_children(a, y)
    if parent is None:
        t = x
    return t

# insertion and deletion

def rb_insert(t, key): #returns the new root
    root = t
    x = Node(key)
    parent = None
    while(t):
        parent = t
        if(key < t.key):
            t = t.left
        else:
            t = t.right
    if parent is None: #tree is empty
        root = x
    elif key < parent.key:
        parent.set_left(x)
    else:
        parent.set_right(x)
    return rb_insert_fix(root, x)

# Fix the red->red violation
def rb_insert_fix(t, x):
    while(x.parent and x.parent.color==RED):
        if x.uncle().color == RED:
            #case 1: ((a:R x:R b) y:B c:R) ==> ((a:R x:B b) y:R c:B)
            set_color([x.parent, x.grandparent(), x.uncle()],
                      [BLACK, RED, BLACK])
            x = x.grandparent()
        else:
            if x.parent == x.grandparent().left:
                if x == x.parent.right:
                    #case 2: ((a x:R b:R) y:B c) ==> case 3
                    x = x.parent
                    t=left_rotate(t, x)
                # case 3: ((a:R x:R b) y:B c) ==> (a:R x:B (b y:R c))
                set_color([x.parent, x.grandparent()], [BLACK, RED])
                t=right_rotate(t, x.grandparent())
            else:
                if x == x.parent.left:
                    #case 2': (a x:B (b:R y:R c)) ==> case 3'
                    x = x.parent
                    t = right_rotate(t, x)
                # case 3': (a x:B (b y:R c:R)) ==> ((a x:R b) y:B c:R)
                set_color([x.parent, x.grandparent()], [BLACK, RED])
                t=left_rotate(t, x.grandparent())
    t.color = BLACK
    return t

# Querying function for deletion, we can reuse those in binary search tree (bstree.py)

def tree_search(t, x):
    while(t!=None and t.key != x):
        if(x < t.key): t = t.left
        else: t = t.right
    return t

def tree_min(t):
    while(t!=None and t.left != None):
        t = t.left
    return t

def remove_node(x):
    if (x is None): return
    x.parent = x.left = x.right = None

def is_leaf(x):
    if x is None: return False
    return (x.left is None) and (x.right is None)

def make_black(parent, x):
    if x is None:
        if is_leaf(parent):
            parent.color = DOUBLY_BLACK
        return parent
    else:
        x.color = x.color + 1
        return x

def rb_delete(t, x):
    if x is None: return t
    (parent, db) = (x.parent, None)
    if x.left is None:
        x.replace_by(x.right)
        db=x.right
    elif x.right is None:
        x.replace_by(x.left)
        db=x.left
    else:
        y = tree_min(x.right)
        (parent, db)=(y.parent, y.right)
        x.key = y.key
        y.replace_by(y.right)
        x=y
    if x.color == BLACK:
        t=rb_delete_fix(t, make_black(parent, db))
    remove_node(x)
    return t

def is_red(x):
    if x is None: return False
    return x.color == RED

def is_black(x):
    if x is None: return False
    return x.color == BLACK

def rb_delete_fix(t, db):
    if db is None: return None # remove the root from a leaf tree
    while(db!=t and db.color==DOUBLY_BLACK):
        if db.sibling() != None:
            # case 1:  the sibling is red, (transform to make the sibling black)
            if is_red(db.sibling()):
                set_color([db.parent, db.sibling()],[RED, BLACK])
                if(db == db.parent.left):
                    t=left_rotate(t, db.parent)
                else:
                    t=right_rotate(t, db.parent)
            # case 3, 4: the sibling is black, and one nephew is red
            elif is_black(db.sibling()) and is_red(db.sibling().left):
                if db == db.parent.left:
                    colors=[BLACK, BLACK, db.parent.color]
                    set_color([db, db.parent, db.sibling().left], colors)
                    t=right_rotate(t, db.sibling())
                    t=left_rotate(t, db.parent)
                else:
                    colors=[BLACK, BLACK, db.parent.color, BLACK]
                    set_color([db, db.parent, db.sibling(), db.sibling().left], colors)
                    t=right_rotate(t, db.parent)
            elif is_black(db.sibling()) and is_red(db.sibling().right):
                if db == db.parent.left:
                    colors=[BLACK, BLACK, db.parent.color, BLACK]
                    set_color([db, db.parent, db.sibling(), db.sibling().right], colors)
                    t=left_rotate(t, db.parent)
                else:
                    colors=[BLACK, BLACK, db.parent.color]
                    set_color([db, db.parent, db.sibling().right], colors)
                    t=left_rotate(t, db.sibling())
                    t=right_rotate(t, db.parent)
            # case 2: the sibling and both nephews are black. (move the blackness up)
            elif is_black(db.sibling()) and (not is_red(db.sibling().left)) and (not is_red(db.sibling().right)):
               set_color([db, db.sibling()], [BLACK, RED])
               db.parent.color=db.parent.color+1
               db = db.parent
            # a sibling without child is invalid case, because it violate property 5
        else: # no sibling, we can move blackness up
            db.color = BLACK
            db.parent.color = db.parent.color+1
            db = db.parent
    t.color=BLACK
    return t

# Helper functions for test

def rbtree_clone(t):
    n = None
    if t != None:
        n = Node(t.key, t.color)
        n.set_children(rbtree_clone(t.left), rbtree_clone(t.right))
    return n

def rbtree_to_str(t):
    if t is None:
        return "."
    else:
        color = {RED:"R", BLACK:"B"}
        return "("+rbtree_to_str(t.left)+ " " + str(t.key) +":"+color[t.color]+" " + rbtree_to_str(t.right)+")"

def list_to_tree(l):
    tree = None
    for x in l:
        tree = rb_insert(tree, x)
    return tree

class Test:
    def __init__(self):
        #t1 = ((1B 2R (4B 3R .)) 5B (6B 7R (8R 9B .)))
        self.t1=Node(5, BLACK)
        self.t1.set_children(Node(2), Node(7))
        self.t1.left.set_children(Node(1, BLACK), Node(4, BLACK))
        self.t1.right.set_children(Node(6, BLACK), Node(9, BLACK))
        self.t1.left.right.set_left(Node(3))
        self.t1.right.right.set_left(Node(8))
        print "t1 1..9:\n", rbtree_to_str(self.t1)
        self.t2=Node(11, BLACK) # as figure 13.4 in CLRS
        self.t2.set_children(Node(2), Node(14, BLACK))
        self.t2.left.set_children(Node(1, BLACK), Node(7, BLACK))
        self.t2.right.set_right(Node(15))
        self.t2.left.right.set_children(Node(5), Node(8))
        print "t2, CLRS fig 13.4:\n", rbtree_to_str(self.t2)

    def __assert(self, msg, x, y):
        if(x == y): msg = msg + "OK."
        else: msg = msg + str(x) + "!=" + str(y) + "Fail."
        print msg

    def run(self):
        self.test_rotate()
        self.test_insert()
        self.test_delete()

    def test_rotate(self):
        t = rbtree_clone(self.t1)
        x = t.right #7R
        t = left_rotate(t, x) #(6 7 (8 9 .)) ==> ((6 7 8) 9 .)
        print "left rotate at 7:R\n", rbtree_to_str(t)
        t = right_rotate(t, t.right) #rotate back
        print "right rotate back:\n", rbtree_to_str(t)
        t = rbtree_clone(self.t1)
        t = left_rotate(t, t) #(2 5 (6 7 9)) ==> ((2 5 6) 7 9)
        print "left rotate at root:\n", rbtree_to_str(t)
        t = right_rotate(t, t) #rotate back
        print "right rotate back:\n", rbtree_to_str(t)

    def test_insert(self):
        t = rbtree_clone(self.t2)
        t = rb_insert(t, 4)
        print "t2: after insert 4\n", rbtree_to_str(t)
        t = list_to_tree([5, 2, 7, 1, 4, 6, 9, 3, 8])
        print "list->tree, create t1 by insert\n", rbtree_to_str(t)

    def __test_del_n(self, tree, n):
        t = rbtree_clone(tree)
        t = rb_delete(t, tree_search(t, n))
        print "del ", n, ": ", rbtree_to_str(t)
        self.__assert("search after del: ", tree_search(t, n), None)

    def test_delete(self):
        for i in range(1, 10):
            self.__test_del_n(self.t1, i)
        self.__test_del_n(self.t1, 11) #del a non-exist value
        t = Node(1, BLACK) #leaf case
        self.__test_del_n(t, 1)
        # test case 2
        t = Node(7, BLACK)
        t.set_children(Node(3, BLACK), Node(10, BLACK))
        t.left.set_children(Node(2, BLACK), Node(5, BLACK))
        t.right.set_children(Node(9, BLACK), Node(12, BLACK))
        t.left.left.set_left(Node(1, BLACK))
        t.left.right.set_children(Node(4, BLACK), Node(6, BLACK))
        t.right.left.set_left(Node(8, BLACK))
        t.right.right.set_left(Node(11, BLACK))
        print "test detailed case...\n", rbtree_to_str(t)
        self.__test_del_n(t, 1)
        # test no sibling case
        t.left.set_right(None)
        print "test no sibling case...\n", rbtree_to_str(t)
        self.__test_del_n(t, 1)
        # test case 1
        t=Node(3, BLACK)
        t.set_children(Node(2, BLACK), Node(6))
        t.left.set_left(Node(1, BLACK))
        t.right.set_children(Node(5, BLACK), Node(7, BLACK))
        t.right.left.set_left(Node(4, BLACK))
        t.right.right.set_right(Node(8, BLACK))
        print "test case 1...\n", rbtree_to_str(t)
        self.__test_del_n(t, 1)
        # test case 3
        t=Node(2, BLACK)
        t.set_children(Node(1, BLACK), Node(6, BLACK))
        t.left.set_left(Node(0, BLACK))
        t.right.set_children(Node(4), Node(7, BLACK))
        t.right.left.set_children(Node(3, BLACK), Node(5, BLACK))
        print "test case 3", rbtree_to_str(t)
        self.__test_del_n(t, 0)
        # test case 4
        t=Node(6, BLACK)
        t.set_children(Node(4, BLACK), Node(7, BLACK))
        t.left.set_children(Node(2), Node(5, BLACK))
        t.right.set_right(Node(8, BLACK))
        t.left.left.set_children(Node(1, BLACK), Node(3, BLACK))
        print "test case 4", rbtree_to_str(t)
        self.__test_del_n(t, 8)

if __name__ == "__main__":
    Test().run()

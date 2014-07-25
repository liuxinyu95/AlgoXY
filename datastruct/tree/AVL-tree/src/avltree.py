#!/usr/bin/python

# avltree.py
# Copyright (C) 2011 Liu Xinyu (liuxinyu95@gmail.com)
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

import random # for test purpose
import string # for debuging purpose

# basicly as same as BST, except there is a balance factor, delta.
class Node:
    def __init__(self, key):
        self.key = key
        self.delta = 0
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

# rotations

# reuse of the same rotation operations as in Red-black tree.
# note that, rotation doesn't update delta at all.

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

# insertion

# top-down insert
def avl_insert(t, key):
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
    return avl_insert_fix(root, x)

# bottom-up update delta and fixing
# params: 
#    t: the root of the tree
#    x: the sub tree which height increases
def avl_insert_fix(t, x):
    #
    # denote d = delta(t), d' = delta(t'), 
    #   where t' is the new tree after insertion.
    #
    # case 1: |d| == 0, |d'| == 1, height increase, 
    #    we need go on bottom-up updating.
    #
    # case 2: |d| == 1, |d'| == 0, height doesn't change,
    #    program terminate
    #
    # case 3: |d| == 1, |d'| == 2, AVL violation,
    #    we need fixing by rotation.
    #
    while x.parent is not None:
        d2 = d1 = x.parent.delta
        if x == x.parent.left:
            d2 = d2 - 1
        else:
            d2 = d2 + 1
        x.parent.delta = d2
        (p, l, r) = (x.parent, x.parent.left, x.parent.right)
        if abs(d1) == 1 and abs(d2) == 0:
            return t
        elif abs(d1) == 0 and abs(d2) == 1:
            x = x.parent
        elif abs(d1)==1 and abs(d2) == 2:
            if d2 == 2:
                if r.delta == 1:  # Right-right case
                    p.delta = 0
                    r.delta = 0
                    t = left_rotate(t, p)
                if r.delta == -1: # Right-Left case
                    dy = r.left.delta
                    if dy == 1: 
                        p.delta = -1
                    else:
                        p.delta = 0
                    r.left.delta = 0
                    if dy == -1:
                        r.delta = 1
                    else:
                        r.delta = 0
                    t = right_rotate(t, r)
                    t = left_rotate(t, p)
            if d2 == -2:
                if l.delta == -1: # Left-left case
                    p.delta = 0
                    l.delta = 0
                    t = right_rotate(t, p)
                if l.delta == 1: # Left-right case
                    dy = l.right.delta
                    if dy == 1:
                        l.delta = -1
                    else:
                        l.delta = 0
                    l.right.delta = 0
                    if dy == -1:
                        p.delta = 1
                    else:
                        p.delta = 0
                    t = left_rotate(t, l)
                    t = right_rotate(t, p)
            break
        else:
            print "shouldn't be there! d1=", d1, "d2=", d2
            assert(False)
    return t

# helpers

def to_list(t):
    if t is None:
        return []
    else:
        return to_list(t.left)+[t.key]+to_list(t.right)

def to_tree(l):
    return reduce(avl_insert, l, None)

def to_str(t):
    if t is None:
        return "."
    else:
        return "(" + to_str(t.left)+ " " + str(t.key) + ":" + str(t.delta) + " " + to_str(t.right)+ ")"

def height(t):
    if t is None:
        return 0
    else:
        return 1 + max(height(t.left), height(t.right))

def is_avl(t):
    if t is None:
        return True
    else:
        delta = height(t.right) - height(t.left)
        return is_avl(t.left) and is_avl(t.right) and abs(delta)<=1

def is_bst(t, xs):
    return to_list(t) == sorted(xs)

def test_insert():
    print "test insert..."
    for _ in range(1000):
        n = random.randint(0, 1000)
        k = random.randint(0, n)
        xs = random.sample(range(0, n), k)
        t = to_tree(xs)
        __assert2(is_bst, t, xs)
        __assert(is_avl, t)
    print "OK"

def __assert(f, t):
    if not f(t):
        print to_str(t)
    assert(f(t))

def __assert2(f, t, xs):
    if not f(t, xs):
        print "xs=", xs
        print "t=", t
        assert(False)

def test():
    test_insert()
                    
if __name__ == "__main__":
    test()

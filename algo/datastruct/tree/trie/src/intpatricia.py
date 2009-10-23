#!/usr/bin/python

import string

class IntTree:
    def __init__(self, key = None, value = None):
        self.key = key
        self.value = value
        self.prefix = self.mask = None
        self.left = self.right = None

    def set_children(l, r):
        self.left = l
        self.right = r

    def replace_child(x, y):
        if self.left == x:
            self.left = y
        else:
            self.right = y

    def is_leaf(self):
        return self.left is None and self.right is None

def maskbit(x, mask):
    pass

def match(key, tree):
    if tree.is_leaf():
        return False
    return maskbit(key, tree.mask) == tree.prefix

def zero(x, mask):
    pass

def branch(t1, t2):
    t = IntTree()
    #need cover cases: t1 is leaf, or t2 is leaf
    (t.prefix, t.mask) = lcp(t1.prefix, t2.prefix)
    if zero(t1.prefix, t.mask):
        t.set_children(t1, t2)
    else:
        t.set_children(t2, t1)
    return t

def insert(t, key, value = None):
    if t is None:
        t = IntTree(key, value)
        return t

    node = t
    parent = None
    while(True):
        if match(key, node):
            parent = node
            if zero(key, node.mask):
                node = node.left
            else:
                node = node.right
        else:
            if node.is_leaf() and key == node.key:
                node.value = value
            else:
                new_node = branch(node, IntTree(key, value))
                if parent is None:
                    t = new_node
                else:
                    parent.replace_child(node, new_node)
            break
    return t

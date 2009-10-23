#!/usr/bin/python

import string

class IntTree:
    def __init__(self, key = None, value = None):
        self.key = key
        self.value = value
        self.prefix = self.mask = 0
        self.left = self.right = None

    def is_leaf(self):
        return self.left is None and self.right is None

def insert(t, key, value = None):
    if t is None:
        t = IntTree(key, value)
        return t

    if t.is_leaf():
        if key == t.key:
            t.value=value
        else:
            t = join(IntTree(key, value), t)
        return t

    node = t
    while(True):
        if match(key, node):
            if key = 

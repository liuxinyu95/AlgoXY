#!/usr/bin/python

# bstree.py
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
# 

# I tested that python doesn't support tail recursion.
# as below:
#
# Program A
#
# def fsum(n):
#     if(n==0):
#         return 0
#     else:
#         return n+fsum(n-1)
#
# Program B
#
# def fact(n, acc = 1):
#        if n == 0:
#                return acc
#        else:
#                return fact(n-1, acc*n)  
# 
# if we call fsum(1000) or fact(1000), the error is:
# RuntimeError: maximum recursion depth exceeded

import sys
import string

# Definition

class Node:
    def __init__(self, key):
        self.key = key
        self.left = None
        self.right = None
        self.parent = None #optional, it's helpful for succ/pred

def in_order_walk(t, f):
    if(t!=None):
        in_order_walk(t.left, f)
        f(t.key)
        in_order_walk(t.right, f)

# Querying

def tree_search(t, x):
    while(t!=None and t.key != x):
        if(x < t.key): t = t.left
        else: t = t.right
    return t

def tree_min(t):
    while(t!=None and t.left != None):
        t = t.left
    return t

def tree_max(t):
    while(t!=None and t.right != None):
        t = t.right
    return t

def succ(x):
    if x is None: return None
    if x.right is not None: return tree_min(x.right)
    p = x.parent
    while p is not None and p.left != x:
        x = p
        p = p.parent
    return p

def pred(x):
    if x is None: return None
    if x.left is not None: return tree_max(x.left)
    p = x.parent
    while p is not None and p.right != x:
        x = p
        p = p.parent
    return p

# Insertion and deletion

def tree_insert(t, key):
    root = t
    x = Node(key)
    parent = None
    while(t):
        parent = t
        if(key < t.key):
            t = t.left
        else:
            t = t.right
    x.parent = parent
    if(parent == None): #tree is empty
        return x
    elif(key < parent.key):
        parent.left = x
    else:
        parent.right = x
    return root

def remove_node(x):
    if (x is None): return
    x.parent = x.left = x.right = None

def tree_delete(t, x):
    if x is None: 
        return t
    [root, old_x, parent] = [t, x, x.parent]
    if x.left is None:
        x = x.right
    elif x.right is None:
        x = x.left
    else:
        y = tree_min(x.right)
        x.key = y.key
        if y.parent != x:
            y.parent.left = y.right
        else:
            x.right = y.right
        remove_node(y)
        return root
    if x is not None:
        x.parent = parent
    if parent is None:
        root = x
    else:
        if parent.left == old_x: 
            parent.left = x
        else: 
            parent.right = x
    remove_node(old_x)
    return root

# Helper functions

def list_to_tree(l):
    tree = None
    for x in l:
        tree = tree_insert(tree, x)
    return tree

def clone_tree(t, parent = None):
    n = None
    if(t != None):
        n = Node(t.key)
        n.left = clone_tree(t.left, n)
        n.right = clone_tree(t.right, n)
        n.parent = parent
    return n

def tree_to_str(t):
    if(t!=None):
        return "("+tree_to_str(t.left)+"), " + str(t.key) + ", (" + tree_to_str(t.right)+")"
    else:
        return "empty"

def my_print(x):
    print x

class Test:
    def __init__(self):
        self.tree = list_to_tree([15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9])
        print tree_to_str(self.tree)

    def __assert(self, msg, x, y):
        if(x == y): msg = msg + "OK."
        else: msg = msg + str(x) + "!=" + str(y) + "Fail."
        print msg
    
    def run(self):
        self.test_in_order_walk()
        self.test_search()
        self.test_min_max()
        self.test_succ_pred()
        self.test_del()

    def test_in_order_walk(self):
        print "test in order walk with my_print: "
        in_order_walk(self.tree, my_print)

    def test_search(self):
        self.__assert("search empty: ", tree_search(None, 3), None)
        print tree_to_str(tree_search(self.tree, 18))
        self.__assert("search non-exist: ", tree_search(self.tree, 5), None)

    def test_min_max(self):
        self.__assert("min(empty): ", tree_min(None), None)
        self.__assert("min(tree): ", tree_min(self.tree).key, 2)
        self.__assert("max(empty): ", tree_max(None), None)
        self.__assert("max(tree): ", tree_max(self.tree).key, 20)

    def test_succ_pred(self):
        self.__assert("succ of 7: ", succ(tree_search(self.tree, 7)).key, 9)
        self.__assert("succ of 13: ", succ(tree_search(self.tree, 13)).key, 15)
        self.__assert("succ of 20: ", succ(tree_search(self.tree, 20)), None)
        self.__assert("pred of 6: ", pred(tree_search(self.tree, 6)).key, 4)
        self.__assert("pred of 7: ", pred(tree_search(self.tree, 7)).key, 6)
        self.__assert("pred of 2: ", pred(tree_search(self.tree, 2)), None)

    def __test_del_n(self, n):
        t = clone_tree(self.tree)
        t = tree_delete(t, tree_search(t, n))
        print "del ", n, ": ", tree_to_str(t)
        self.__assert("search after del: ", tree_search(t, n), None)

    def test_del(self):
        self.__test_del_n(17)
        self.__test_del_n(7)
        self.__test_del_n(6)
        self.__test_del_n(15)
        self.__test_del_n(1) #del a non-exist value

if __name__ == "__main__":
    Test().run()

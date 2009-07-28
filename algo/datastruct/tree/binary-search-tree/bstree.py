# I tested that python doesn't support tail recursion.
# as below:
# def fsum(n):
#     if(n==0):
#         return 0
#     else:
#         return n+fsum(n-1)
# if we call fsum(1000), the error is:
# RuntimeError: maximum recursion depth exceeded

import sys
import string

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

def list_to_tree(l):
    tree = None
    for x in l:
        tree = tree_insert(tree, x)
    return tree

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

    def run(self):
        self.test_in_order_walk()

    def test_in_order_walk(self):
        print "test in order walk with my_print: "
        in_order_walk(self.tree, my_print)

if __name__ == "__main__":
    Test().run()

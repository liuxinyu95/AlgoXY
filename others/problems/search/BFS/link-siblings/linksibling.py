#!/usr/bin/python

# Given a complete binary tree, link all siblings in the same level with constant space
# example:
#
#     a
#    /  \
#   b    c
#  / \  / \
# d  e  f  g
#
# after linking, the tree transformed to:
#
#     a
#    /  \
#   b -> c
#  / \  / \
# d->e->f->g
#

class Node:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None
        self.next = None

def node(k, l = None, r = None):
    tr = Node(k)
    tr.left = l
    tr.right = r
    return tr

def output(tr):
    if tr:
        print tr.val, "->", (tr.next.val if tr.next else "NIL")
        output(tr.left)
        output(tr.right)

class Solution:
    # @param root, a tree node
    # @return nothing
    def connect(self, root):
        link(root)

# BFS like traverse
def link(tr):
    if tr is None or tr.left is None:
        return
    (root, tr.next) = (tr, tr.left)
    (head, tail) = (tr, tr)
    while head.next:
        tr, head = deq(head)
        tail = enq(tail, tr.left)
        tail = enq(tail, tr.right)
    # break at 1, 2, 4, ...
    (i, m) = (0, 1)
    prev = tr = root
    while tr:
        (prev, tr) = (tr, tr.next)
        i = i + 1
        if i == m:
            prev.next = None
            (i, m) = (0, 2 * m)

def deq(head):
    return head, head.next

def enq(tail, x):
    if x:
        tail.next = x
        return x
    else:
        return tail

def test():
    #tr = None
    #tr = node(1)
    #tr = node(1, node(2), node(3))
    tr = node(1, node(2, node(4), node(5)), node(3, node(6), node(7)))
    link(tr)
    output(tr)

if __name__ == "__main__":
    test()

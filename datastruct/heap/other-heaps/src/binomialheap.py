#!/usr/bin/python

# binomialheap.py, binomial heap 
# Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)
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

import random # for testing only

# Assume the heap is min-heap

# Use left child, right sibling approach
class BinomialTree:
    def __init__(self, x = None):
        self.rank = 0
        self.key = x
        self.parent = None
        self.child = None
        self.sibling = None

# Auxiliary function to extract the first tree
def extract_first(h):
    t = None
    if h is not None:
        t = h
        h = h.sibling
        t.sibling = None
    return (t, h)

# Implicit condition that the rank of the two trees are same       
def link(t1, t2):
    if t2.key < t1.key:
        (t1, t2) = (t2, t1)
    t2.sibling = t1.child
    t1.child = t2
    t2.parent = t1
    t1.rank = t1.rank + 1
    return t1

# Insert a tree to the proper position in the heap
# So that the trees are in monotonically increase order by rank
# Implicit condition: the rank of tree is lower or equal to the
# first tree in the heap
def insert_tree(h, t):
    while h is not None and t.rank == h.rank:
        (t1, h) = extract_first(h)
        t = link(t, t1)
    t.sibling = h
    return t

# Insertion
def insert(h, x):
    return insert_tree(h, BinomialTree(x))

# Append a tree to the heap, so that the trees are in
# monotonically increase order by rank
# Implicit condition: the rank of tree is equal to or bigger by 1 than
# the last tree in the heap.
# Because the tail tree in the heap may be changed, we need the 2nd last
# tree as the argument
def append_tree(head, prev, tail, x):
    if head is None:
        return (x, None, x)
    if tail.rank == x.rank:
        tail = link(tail, x)
        if prev is None:
            return (tail, None, tail)
        prev.sibling = tail
    else:
        tail.sibling = x
        prev = tail
        tail = x
    return (head, prev, tail)

# Helper function to append a heap to another one by repeatedly calling
# append_tree()
def append_trees(h, p, t, xs):
    while xs is not None:
        (x, xs) = extract_first(xs)
        (h, p, t) = append_tree(h, p, t, x)
    return (h, p, t)

# Merge 2 heaps together. Use a merge sort like approach
def merge(h1, h2):
    if h1 is None:
        return h2
    if h2 is None:
        return h1
    (h, p, t) = (None, None, None)
    while h1 is not None and h2 is not None:
        x = None
        if h1.rank < h2.rank:
            (x, h1) = extract_first(h1)
        elif h2.rank < h1.rank:
            (x, h2) = extract_first(h2)
        else:
            (x1, h1) = extract_first(h1)
            (x2, h2) = extract_first(h2)
            x = link(x1, x2)
        (h, p, t) = append_trees(h, p, t, x)
    if h1 is not None:
        (h, p, t) = append_trees(h, p, t, h1)
    if h2 is not None:
        (h, p, t) = append_trees(h, p, t, h2)
    return h

# Reverse the linked list
def reverse(h):
    prev = None
    while h is not None:
        x = h
        h = h.sibling
        x.sibling = prev
        prev = x
    return prev

# Extract the minimum binomial tree from the heap
# returns (min tree, rest trees)
def remove_min_tree(h):
    head = h
    (prev_min, min_t) = (None, None)
    prev = None
    while h is not None:
        if min_t is None or h.key < min_t.key:
            min_t = h
            prev_min = prev
        prev = h
        h = h.sibling
    if prev_min is not None:
        prev_min.sibling = min_t.sibling
    else:
        head = min_t.sibling
    min_t.sibling = None
    return (min_t, head)    

# Assume h is not empty
def find_min(h):
    min_t = None
    while h is not None:
        if min_t is None or h.key < min_t.key:
            min_t = h
        h = h.sibling
    return min_t.key

# Extract the min element, returns the (min, heap')
def extract_min(h):
    (min_t, h) = remove_min_tree(h)
    h = merge(h, reverse(min_t.child))
    min_t.child = None
    return (min_t.key, h)

def decrease_key(x, k):
    assert(k < x.key)
    x.key = k
    p = x.parent
    while p is not None and x.key < p.key:
        (x.key, p.key) = (p.key, x.key)
        x = p
        p = p.parent

# Delete node is trivial so I skip it in this program
# A reference implementation:
# function delete_node(h, x)
#   decrease_key(x, -infinity)
#   (_, h) = extract_min(h)
#   return 

# helper function
def from_list(lst):
    return reduce(insert, lst, None)

def heap_sort(lst):
    h = from_list(lst)
    res = []
    while h is not None:
        (x, h) = extract_min(h)
        res.append(x)
    return res

def to_string(h):
    s = ""
    while h is not None:
        s = s+ "(" + str(h.key)+", "+to_string(h.child)+"), "
        h = h.sibling
    return s

# Auxiliary function to find a node contains specified 
# key in the heap. This is an inefficent function only
# for verification purpose
def find_key(h, k):
    while h is not None:
        if h.key == k:
            return h
        else:
            n = find_key(h.child, k)
            if n is not None:
                return n
        h = h.sibling
    return None

def decrease_key_from(h, k1, k2):
    decrease_key(find_key(h, k1), k2)

class TestHeap:
    def __init__(self):
        print "Binomial heap testing"

    def run(self):
        #self.test_insert()
        #self.test_extract_min()
        self.test_heap_sort()
        self.test_random_sort()
        self.test_heap_decrease_key()

    def __assert(self, p):
        if p:
            print "OK"
        else:
            print "Fail!"

    def test_insert(self):
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        print to_string(from_list(l))

    def test_extract_min(self):
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        h = from_list(l)
        (t, h) = extract_min(h)
        print "t=", t
        print "h=", to_string(h)

    def test_heap_sort(self):
        # CLRS Figure 6.4
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        res = heap_sort(l)
        print res
        self.__assert(res == [1, 2, 3, 4, 7, 8, 9, 10, 14, 16])

    def test_random_sort(self):
        n = 1000
        for i in range(100):
            lst = random.sample(range(n), random.randint(1, n))
            assert(heap_sort(lst) == sorted(lst))
        print "OK"
        
    def test_heap_decrease_key(self):
        n = 1000
        for i in range(100):
            lst = random.sample(range(n), random.randint(1, n))
            h = from_list(lst)
            x = random.choice(lst)
            y = x - random.randint(0, x) - 1
            decrease_key_from(h, x, y)
            res = []
            while h is not None:
                (e, h) = extract_min(h)
                res.append(e)
            lst[lst.index(x)] = y
            assert(res == sorted(lst))
        print "OK"

if __name__ == "__main__":
    TestHeap().run()

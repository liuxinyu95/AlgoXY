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

def remove_first(h):
    next = h.sibling
    h.sibling = None
    return next

def link(t1, t2):
    assert(t1.rank == t2.rank)
    if t2.key < t1.key:
        (t1, t2) = (t2, t1)
    t2.sibling = t1.child
    t1.child = t2
    t2.parent = t1
    t1.rank = t1.rank + 1
    return t1

def insert_tree(h, t):
    h1 = prev = BinomialTree()
    while h and h.rank <= t.rank:
        t1 = h
        h = h.sibling
        if t.rank == t1.rank:
            t = link(t, t1)
        else:
            prev.sibling = t1
            prev = t1
    prev.sibling = t
    t.sibling = h
    return remove_first(h1)

def insert(h, x):
    return insert_tree(h, BinomialTree(x))

def merge(h1, h2):
    h = prev = BinomialTree()
    while h1 and h2:
        if h1.rank < h2.rank:
            prev.sibling = h1
            prev = prev.sibling
            h1 = h1.sibling
        elif h2.rank < h1.rank:
            prev.sibling = h2
            prev = prev.sibling
            h2 = h2.sibling
        else:
            (t1, t2) = (h1, h2)
            (h1, h2) = (h1.sibling, h2.sibling)
            h1 = insert_tree(h1, link(t1, t2))
    if h1:
        prev.sibling = h1
    if h2:
        prev.sibling = h2
    return remove_first(h)

def reverse(h):
    prev = None
    while h:
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
    while h:
        if min_t is None or h.key < min_t.key:
            min_t = h
            prev_min = prev
        prev = h
        h = h.sibling
    if prev_min:
        prev_min.sibling = min_t.sibling
    else:
        head = min_t.sibling
    min_t.sibling = None
    return (min_t, head)

# Assume h is not empty
def find_min(h):
    min_t = None
    while h:
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
    while p and x.key < p.key:
        (x.key, p.key) = (p.key, x.key)
        x = p
        p = p.parent

# A reference implementation for delete node:
# function delete_node(h, x)
#   decrease_key(x, -infinity)
#   (_, h) = extract_min(h)
#   return

def from_list(xs):
    t = None
    for x in xs:
        t = insert(t, x)
    return t

def heap_sort(lst):
    h = from_list(lst)
    res = []
    while h:
        (x, h) = extract_min(h)
        res.append(x)
    return res

def to_string(h):
    s = ""
    while h:
        s = s+ "(" + str(h.key)+", "+to_string(h.child)+"), "
        h = h.sibling
    return s

# Auxiliary function to find a node contains specified
# key in the heap. This is an inefficent function only
# for verification purpose
def find_key(h, k):
    while h:
        if h.key == k:
            return h
        else:
            n = find_key(h.child, k)
            if n:
                return n
        h = h.sibling
    return None

def decrease_key_from(h, k1, k2):
    decrease_key(find_key(h, k1), k2)

class TestHeap:
    def __init__(self):
        print("Binomial heap testing")

    def run(self):
        self.test_insert()
        self.test_extract_min()
        self.test_heap_sort()
        self.test_random_sort()
        self.test_heap_decrease_key()

    def __assert(self, p):
        if p:
            print("OK")
        else:
            print("Fail!")

    def test_insert(self):
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        print("insert", l, "=", to_string(from_list(l)))

    def test_extract_min(self):
        print("text extract min")
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        h = from_list(l)
        (t, h) = extract_min(h)
        print("t=", t)
        print("h=", to_string(h))

    def test_heap_sort(self):
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        res = heap_sort(l)
        print("test heap sort:", res)
        assert(res == [1, 2, 3, 4, 7, 8, 9, 10, 14, 16])

    def test_random_sort(self):
        print("test random sort")
        n = 1000
        for i in range(100):
            lst = random.sample(range(n), random.randint(1, n))
            assert(heap_sort(lst) == sorted(lst))
        print("OK")

    def test_heap_decrease_key(self):
        print("test decrease key")
        n = 1000
        for i in range(100):
            lst = random.sample(range(n), random.randint(1, n))
            h = from_list(lst)
            x = random.choice(lst)
            y = x - random.randint(0, x) - 1
            decrease_key_from(h, x, y)
            res = []
            while h:
                (e, h) = extract_min(h)
                res.append(e)
            lst[lst.index(x)] = y
            assert(res == sorted(lst))
        print("OK")

if __name__ == "__main__":
    TestHeap().run()

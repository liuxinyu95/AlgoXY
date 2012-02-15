#!/usr/bin/python

# pairingheap.py, pairing heap 
# Copyright (C) 2012, Liu Xinyu (liuxinyu95@gmail.com)
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
# Based on Michael L. Fredman, Robert Sedgewick, Daniel D. Sleator, 
# and Robert E. Tarjan. ``The Pairing Heap: A New Form of Self-Adjusting 
# Heap'' Algorithmica (1986) 1: 111-129
#

import random # for testing only
from collections import deque # for right to left merge only

#
# Assume the heap is min-heap
#   If we don't realize decrease-key operation, there is no need
#   to use the parent pointer.
#
class KTree:
    def __init__(self, x = None):
        self.key = x
        self.parent = None
        self.children = []

# O(1) time merge two heaps
#
# Instead of add one tree as the first child, it is added at
# the end of the children list to achieve O(1) time if the
# built-in list is realized as array.
# However, it should be insert at the beginning if it is realized
# as linked list.
def merge(t1, t2):
    if t1 is None:
        return t2
    if t2 is None:
        return t1
    if t2.key < t1.key:
        (t1, t2) = (t2, t1)
    t1.children.append(t2)
    t2.parent = t1
    return t1

def insert(h, x):
    return merge(h, KTree(x))

def insert_node(h, x):
    return merge(h, x)

def top(h):
    return h.key

def decrease_key(h, x, key):
    x.key = key # assume key <= x.key
    if x.parent is not None:
        x.parent.children.remove(x) # Sadly, this is O(N) operation.
    x.parent = None
    return merge(x, h)

# Python itertools and receipe provide plenty of
# tools, which help for iterating over pairs.
# They are not used here, so fresh python user
# can read the code.
def pop(h):
    lst = deque()
    x = None
    for y in h.children:
        if x is None:
            x = y
        else:
            lst.appendleft(merge(x, y))
            x = None
    for y in lst:
        x = merge(x, y)
    return x
        
# helper functions
def from_list(lst):
    return reduce(insert, lst, None)

def heap_sort(lst):
    h = from_list(lst)
    res = []
    while h is not None:
        res.append(top(h))
        h = pop(h)
    return res

def to_str(h):
    s = "(" + str(h.key) + ", "
    for t in h.children:
        s = s + to_str(t)
    s = s + ")"
    return s

# testing
def test_sort():
    n = 1000
    for i in range(n):
        lst = random.sample(range(n), random.randint(1, n))
        assert(heap_sort(lst) == sorted(lst))
    print "heap-sort:", n, "test cases are OK."

def test_decrease_key():
    n = 1000
    m = 16 # too slow for big m
    for i in range(m):
        xs = random.sample(range(n), random.randint(1, n))
        ns = [KTree(x) for x in xs]
        h = reduce(insert_node, ns, None)
        xs = [x - random.randint(1, n) for x in xs]
        for node, x in zip(ns, xs):
            h = decrease_key(h, node, x)
        ys = []
        while h is not None:
            ys.append(top(h))
            h = pop(h)
        assert(ys == sorted(xs))
    print "decrease-key:", m, "test cases are OK."

def test():
    test_sort()
    test_decrease_key()

if __name__ == "__main__":
    test()

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

from functools import reduce
from random import sample, randint, choice
from collections import deque # for right to left merge

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
    if x.parent:
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

def lookup_node(h, x):
    if h.key == x:
        return h
    for t in h.children:
        node = lookup_node(t, x)
        if node:
            return node
    return None

def delete(h, x):
    node = lookup_node(h, x)
    if not node:
        return h
    if node == h:
        return pop(h)
    node.parent.children.remove(node)
    node.parent = None
    return merge(pop(node), h)

def from_list(lst):
    return reduce(insert, lst)

def to_list(h):
    xs = []
    while h:
        xs.append(top(h))
        h = pop(h)
    return xs

def heap_sort(lst):
    return to_list(from_list(lst))

def to_str(h):
    s = "(" + str(h.key) + ", "
    for t in h.children:
        s = s + to_str(t)
    s = s + ")"
    return s

# testing
def random_list(n = 1000):
    return sample(range(n), randint(1, n))

def test(f):
    for _ in range(100):
        xs = random_list()
        f(xs)
    print(f"100 tests for {f} passed.")

def test_sort(xs):
    ys = heap_sort(xs)
    zs = sorted(xs)
    assert ys == zs, f"heap sort fail: xs = {xs}, ys = {ys}, zs = {zs}"

def test_decrease_key(xs):
    n = len(xs)
    lst = xs.copy()
    ns = [KTree(x) for x in xs]
    h = reduce(insert_node, ns)
    i = randint(0, n-1)
    xs[i] = xs[i] - randint(1, n)
    h = decrease_key(h, ns[i], xs[i])
    ys = to_list(h)
    zs = sorted(xs)
    assert ys == zs, f"decease-key fail: xs = {lst}, changed to: {xs}, ys = {ys}, zs = {zs}"

def test_del(xs):
    h = from_list(xs)
    n = len(xs)
    y = choice(xs)
    h = delete(h, y)
    ys = to_list(h)
    zs = xs.copy()
    zs.remove(y)
    zs.sort()
    assert ys == zs, f"del fail: xs = {xs}, y = {y}, ys = {ys}, zs = {zs}"

if __name__ == "__main__":
    test(test_sort)
    test(test_decrease_key)
    test(test_del)

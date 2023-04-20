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


# Assume the heap is min-heap
class KTree:
    def __init__(self, x = None):
        self.key = x
        self.parent = None
        self.subtrees = []

# O(1) time merge two heaps
#
# Add to tail (append) instead of insert to the head to achieve O(1) time for array.
# Add to head for linked-list
def merge(t1, t2):
    if t1 is None:
        return t2
    if t2 is None:
        return t1
    if t2.key < t1.key:
        (t1, t2) = (t2, t1)
    t1.subtrees.append(t2)
    t2.parent = t1
    return t1

# mitigate the worst case, that continuously insert n elements, followed with a pop
# operation, when n is big, the pop performance overhead (to amortize) is big.
MAX_SUBTREES = 16

def insert(h, x):
    if h and len(h.subtrees) > MAX_SUBTREES:
        h = insert(pop(h), top(h))
    return merge(h, KTree(x))

def top(h):
    return h.key

def decrease_key(h, tr, key):
    if (not tr) or tr.key < key:
        return h
    tr.key = key
    if tr == h:
        return h
    tr.parent.subtrees.remove(tr) # O(n), where n = len(subtrees)
    tr.parent = None
    return merge(tr, h)

# Alternative: to use itertools and receipe to iterate over pairs
def pop(h):
    lst = deque()
    x = None
    for y in h.subtrees:
        if x is None:
            x = y
        else:
            lst.appendleft(merge(x, y))
            x = None
    for y in lst:
        x = merge(x, y)
    return x

def lookuptr(h, x):
    if h.key == x:
        return h
    for t in h.subtrees:
        tr = lookuptr(t, x)
        if tr:
            return tr
    return None

def delete(h, x):
    tr = lookuptr(h, x)
    if not tr:
        return h
    if tr == h:
        return pop(h)
    tr.parent.subtrees.remove(tr)
    tr.parent = None
    return merge(pop(tr), h)

def from_list(lst):
    return reduce(insert, lst, None)

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
    for t in h.subtrees:
        s = s + to_str(t)
    s = s + ")"
    return s

def test(f, n = 100):
    for _ in range(100):
        xs = sample(range(n), randint(1, n))
        f(xs)
    print(f"{n} tests for {f} passed.")

def test_sort(xs):
    ys = heap_sort(xs)
    zs = sorted(xs)
    assert ys == zs, f"heap sort fail: xs = {xs}, ys = {ys}, zs = {zs}"

def test_decrease_key(xs):
    n = len(xs)
    x = choice(xs)
    y = x - randint(1, n)
    h = from_list(xs)
    h = decrease_key(h, lookuptr(h, x), y)
    ys = to_list(h)
    zs = sorted(y if a == x else a for a in xs)
    assert ys == zs, f"decease-key fail: xs = {xs}, changed from {x} to {y}, ys = {ys}, zs = {zs}"

def test_del(xs):
    h = from_list(xs)
    n = len(xs)
    y = choice(xs)
    h = delete(h, y)
    ys = to_list(h)
    zs = sorted(filter(lambda x: x != y, xs))
    assert ys == zs, f"del fail: xs = {xs}, y = {y}, ys = {ys}, zs = {zs}"

if __name__ == "__main__":
    test(test_sort)
    test(test_decrease_key)
    test(test_del)

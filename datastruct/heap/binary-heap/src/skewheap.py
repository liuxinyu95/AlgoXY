# Copyright (C) 2023, Liu Xinyu (liuxinyu95@gmail.com)
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

from random import sample, randint

class Node:
    def __init__(self, value, left = None, right = None):
        self.value = value
        self.left = left
        self.right = right

def merge(a, b):
    h = Node(None)  # the sentinel node
    root = h
    while a and b:
        if b.value < a.value:
            a, b = b, a
        c = Node(a.value, left = None, right = a.left)
        h.left = c
        h = c
        a = a.right
    h.left = a if a else b
    root = root.left
    return root

def insert(h, x):
    return merge(Node(x), h)

def top(h):
    return h.value

def pop(h):
    return merge(h.left, h.right)

def fromlist(xs):
    h = None
    for x in xs:
        h = insert(h, x)
    return h

def hsort(xs):
    h = fromlist(xs)
    ys = []
    while h:
        ys.append(top(h))
        h = pop(h)
    return ys

def test(f):
    for _ in range(100):
        xs = sample(range(100), randint(0, 100))
        f(xs)
    print(f"100 tests for {f} passed.")

def prop_heap(xs):
    h = fromlist(xs)
    if xs:
        a = top(h)
        b = min(xs)
        assert a == b, f"violate heap: top = {a}, min = {b}"

def prop_heapsort(xs):
    ys = hsort(xs)
    zs = sorted(xs)
    assert ys == sorted(xs), f"violate heap sort, xs = {xs}, ys = {ys}, zs = {zs}"

if __name__ == "__main__":
    test(prop_heap)
    test(prop_heapsort)

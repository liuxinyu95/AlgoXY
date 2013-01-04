#!/usr/bin/python

# list-imp.py
# Copyright (C) 2012 Liu Xinyu (liuxinyu95@gmail.com)
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

# Imperative list manipulations
import random

class List:
    def __init__(self, x = None, xs = None):
        self.key = x
        self.next = xs

def cons(x, xs):
    return List(x, xs)

def length(xs):
    n = 0
    while xs is not None:
        n = n + 1
        xs = xs.next
    return n

def append(xs, x):
    xs = cons(None, xs)
    r = xs
    while xs.next is not None:
        xs = xs.next
    xs.next = List(x)
    return r.next

def mapL(f, xs):
    ys = prev = List()
    while xs is not None:
        prev.next = List(f(xs.key))
        prev = prev.next
        xs = xs.next
    return ys.next

def span(p, xs):
    ys = xs
    last = None
    while xs is not None and p(xs.key):
        last = xs
        xs = xs.next
    if last is None:
        return (None, xs)
    last.next = None
    return (ys, xs)

def group1(p, xs):
    if xs is None:
        return List(None)
    (x, xs) = (xs.key, xs.next)
    g = List(x)
    G = List(g)
    while xs is not None:
        y = xs.key
        if p(x, y):
            g = append(g, y)
        else:
            g = List(y)
            G = append(G, g)
        x = y
        xs = xs.next
    return G

def find(p, xs):
    while xs is not None:
        if p(xs.key):
            return xs
        xs = xs.next
    return None

# auxiliar functions
def fromList(xs):
    ys = None
    for x in reversed(xs):
        ys = cons(x, ys)
    return ys

def toList(xs):
    ys = []
    while xs is not None:
        ys.append(xs.key)
        xs = xs.next
    return ys

# testing
def test_map():
    for _ in range(100):
        lst = random.sample(range(100), random.randint(0, 100))
        assert toList(mapL(lambda x: -x, fromList(lst))) == map(lambda x: -x, lst)

def test_span():
    for _ in range(100):
        lst = range(100)
        random.shuffle(lst)
        (xs, ys) = span(lambda x: x < 50, fromList(lst))
        if xs is not None:
            assert any(toList(mapL(lambda x: x < 50, xs)))
        if ys is not None:
            assert not ys.key < 50
        assert length(xs) + length(ys) == len(lst)

def test_find():
    for _ in range(100):
        lst = random.sample(range(100), random.randint(0, 100))
        x = random.randint(0, 100)
        if x in lst:
            assert x == find(lambda a : x == a, fromList(lst)).key
        else:
            assert None == find(lambda a : x == a, fromList(lst))

def test_group1():
    gs = group1(lambda x, y: x == y, fromList("mississipi"))
    while gs is not None:
        print toList(gs.key),
        gs = gs.next
    print

if __name__ == "__main__":
    test_map()
    test_span()
    test_group1()
    test_find()

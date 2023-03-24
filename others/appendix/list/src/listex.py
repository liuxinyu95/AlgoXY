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

def takeM(n, xs):
    d = len(xs) - n
    while d > 0 and xs:
        xs.pop()
        d = d - 1
    return xs

def dropM(n, xs):
    if n <= 0:
        return xs
    for i in range(n, len(xs)):  # shift all elements ahead n
        xs[i - n] = xs[i]
    while n > 0 and xs:
        xs.pop()
        n = n - 1
    return xs

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

def filterL(p, xs):
    ys = None
    while xs:
        if p(xs.key):
            ys = cons(xs.key, ys)
        xs = xs.next
    return reverse(ys)

def reverse(xs):
    ys = None
    while xs:
        ys = cons(xs.key, ys)
        xs = xs.next
    return ys

def lookupAssoc(x, assoc):
    for k, v in assoc:
        if x == k:
            return v
    return None

def zipL(xs, ys):
    zs = None
    while xs and ys:
        zs = cons((xs.key, ys.key), zs)
        xs = xs.next
        ys = ys.next
    return reverse(zs)

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
def randlist(n):
    return random.sample(range(n), random.randint(0, n))

def test_zip():
    for _ in range(100):
        xs = randlist(100)
        ys = randlist(100)
        assert toList(zipL(fromList(xs), fromList(ys))) == list(zip(xs, ys))
    print("tested zip")

def test_filter():
    even = lambda x: x % 2 == 0
    for _ in range(100):
        lst = randlist(100)
        assert toList(filterL(even, fromList(lst))) == list(filter(even, lst))
    print("tested filter")

def test_map():
    negate = lambda x: -x
    inc = lambda x: 1 + x
    for _ in range(100):
        lst = randlist(100)
        assert all([toList(mapL(f, fromList(lst))) == list(map(f, lst)) for f in [negate, inc, abs]])
    print("tested map")

def test_span():
    for _ in range(100):
        lst = list(range(100))
        random.shuffle(lst)
        (xs, ys) = span(lambda x: x < 50, fromList(lst))
        if xs is not None:
            assert any(toList(mapL(lambda x: x < 50, xs)))
        if ys is not None:
            assert not ys.key < 50
        assert length(xs) + length(ys) == len(lst)
    print("tested span")

def test_find():
    for _ in range(100):
        lst = randlist(100)
        x = random.randint(0, 100)
        if x in lst:
            assert x == find(lambda a : x == a, fromList(lst)).key
        else:
            assert None == find(lambda a : x == a, fromList(lst))
    print("tested find")

def test_group1():
    gs = group1(lambda x, y: x == y, fromList("mississipi"))
    while gs is not None:
        print(toList(gs.key), end = '')
        gs = gs.next
    print("tested groupping")

def test_sublist():
    for _ in range(100):
        xs = randlist(100)
        n = random.randint(-10, 110)
        assert takeM(n, xs) == xs[:n]
        assert dropM(n, xs) == xs[n:]
    print("tested sublist")

if __name__ == "__main__":
    test_map()
    test_span()
    test_group1()
    test_find()
    test_sublist()
    test_filter()
    test_zip()

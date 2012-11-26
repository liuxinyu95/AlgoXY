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

class List:
    def __init__(self, x = None, xs = None):
        self.key = x
        self.next = xs

def cons(x, xs):
    return List(x, xs)

def mapL(f, xs):
    ys = prev = List()
    while xs is not None:
        prev.next = List(f(xs.key))
        prev = prev.next
        xs = xs.next
    return ys.next

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
    assert toList(mapL(lambda x: -x, fromList(range(10)))) == map(lambda x: -x, range(10))

if __name__ == "__main__":
    test_map()

#!/usr/bin/python

# ttree.py
# Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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


# Tournament tree based selection sort
# [1] Donald E. Knuth. ``The Art of Computer Programming, Volume 3: Sorting and Searching (2nd Edition)''. Addison-Wesley Professional; 2 edition (May 4, 1998) ISBN-10: 0201896850 ISBN-13: 978-0201896855

from itertools import izip

N_INF = -10000

class Node:

    def __init__(self, x = None, l = None, r = None):
        self.key = x
        self.left = l 
        self.right = r
        self.parent = None
        if l is not None:
            l.parent = self
        if r is not None:
            r.parent = self

def leaf(t):
    return t is not None and t.left is None and t.right is None

# limitation: |xs| = 2 ^ m for some m in N
def build(xs):
    ts = [ Node(x) for x in xs]
    while len(ts) > 1:
        ts1 = []
        for x, y in zip(*[iter(ts)]*2):
            ts1.append(Node(max(x.key, y.key), x, y))
        ts = ts1
    return ts[0]

# limitation: for all x, x > -inf
def pop(t):
    x = t.key
    t.key = N_INF
    while not leaf(t):
        t = t.left if t.left.key == x else t.right
        t.key = N_INF
    while t.parent is not None:
        t = t.parent
        t.key = max(t.left.key, t.right.key)
    return (x, t)

def empty(t):
    return t.key == N_INF

# testing

# same limitation as build function.
def tsort(xs):
    ys = []
    t = build(xs)
    while not empty(t):
        (x, t) = pop(t)
        ys.append(x)
    return reversed(ys)

def to_str(t):
    return "." if t is None else "(" + to_str(t.left) + " " + str(t.key) + " " + to_str(t.right) + ")"

if __name__ == "__main__":
    xs = [7, 6, 15, 16, 8, 4, 13, 3, 5, 10, 9, 1, 12, 2, 11, 14]
    print to_str(build(xs))
    print "sort:", tsort(xs)

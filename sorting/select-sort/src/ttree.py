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

class Node:

    def __init__(self, x = None, l = None, r = None):
        self.key = x
        self.left = l 
        self.right = r
        self.parent = None

def build(xs):
    ts = [ Node(x) for x in xs]
    while len(ts) > 1:
        ts1 = []
        for x, y in zip(*[iter(ts)]*2):
            ts1.append(Node(max(x.key, y.key), x, y))
        ts = ts1
    return ts[0]

def pop(xs):
    pass

def to_str(t):
    def with_depth(t, depth):
        return "." if t is None else "(" + with_depth(t.left, depth+1) + " " + str(t.key) + "_" + str(depth)+ " " + with_depth(t.right, depth+1) + ")"
    return with_depth(t, 0)

if __name__ == "__main__":
    print to_str(build([7, 6, 14, 16, 8, 4, 13, 3, 5, 10, 9, 1, 12, 2, 11, 15]))

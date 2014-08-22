#!/usr/bin/python

# klotski.py
# Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
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


# `Heng Dao Li Ma' layout
# 1 A A 2
# 1 A A 3
# 3 4 4 5
# 3 7 8 5
# 6 0 0 9

from collections import deque

START = [[(1, 1), (2, 1)],
         [(1, 4), (2, 4)],
         [(3, 1), (4, 1)],
         [(3, 2), (3, 3)],
         [(3, 4), (4, 4)],
         [(5, 1)], [(4, 2)], [(4, 3)], [(5, 4)],
         [(1, 2), (1, 3), (2, 2), (2, 3)]]

class Node:
    def __init__(self, l, p = None):
        self.layout = l
        self.parent = p

def solve(start):
    visit = set([normalize(start)])
    queue = deque([Node(start)])
    while queue:
        cur = queue.popleft()
        layout = cur.layout
        if layout[-1] == [(4, 2), (4, 3), (5, 2), (5, 3)]:
            return cur
        else:
            for brd in expand(layout, visit):
                queue.append(Node(brd, cur))
                visit.add(normalize(brd))
    return None # no solution

def expand(layout, visit):
    def bound(y, x):
        return 1 <= y and y <= 5 and 1 <= x and x <= 4
    def valid(m, i, y, x):
        return m[y - 1][x - 1] in [0, i]
    def unique(brd):
        (m, n) = (normalize(brd), normalize(mirror(brd)))
        return m not in visit and n not in visit
    s = []
    d = [(0, -1), (0, 1), (-1, 0), (1, 0)]
    m = matrix(layout)
    for i in range(1, 11):
        for (dy, dx) in d:
            if all(bound(y + dy, x + dx) and valid(m, i, y + dy, x + dx)
                    for (y, x) in layout[i - 1]):
                brd = move(layout, (i, (dy, dx)))
                if unique(brd):
                    s.append(brd)
    return s

def dup(layout):
    return [r[:] for r in layout]

def matrix(layout):
    m = [[0]*4 for _ in range(5)]
    for (i, ps) in zip(range(1, 11), layout):
        for (y, x) in ps:
            m[y - 1][x - 1] = i
    return m

def move(layout, delta):
    (i, (dy, dx)) = delta
    m = dup(layout)
    m[i - 1] = [(y + dy, x + dx) for (y, x) in m[i - 1]]
    return m

def mirror(layout):
    return [[(y, 5 - x) for (y, x) in r] for r in layout]

def normalize(layout):
    return tuple(sorted([tuple(sorted(r)) for r in layout]))

# pretty print
def output(node):
    seq = []
    while node is not None:
        seq = [node.layout] + seq
        node = node.parent
    for layout in seq:
        for r in matrix(layout):
            print ["%X" % x for x in r]
        print "\n",
    print "total", len(seq) - 1, "steps"

if __name__ == "__main__":
    output(solve(START))

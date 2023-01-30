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
# 1 A A 2       0  1  2  3
# 1 A A 2       4  5  6  7
# 3 4 4 5       8  9  10 11
# 3 7 8 5       12 13 14 15
# 6 0 0 9       16 17 18 19

from collections import deque
from copy import deepcopy

START = [frozenset({0, 4}), frozenset({3, 7}), frozenset({8, 12}),
         frozenset({9, 10}), frozenset({11, 15}), frozenset({16}),
         frozenset({13}), frozenset({14}), frozenset({19}),
         frozenset({1, 2, 5, 6})]

END = frozenset({13, 14, 17, 18})

def posof(c):
    return (c // 4, c % 4)

def mirror(c):
    (y, x) = posof(c)
    return 4 * y + 3 - x

def matrix(layout):
    m = [[0]*4 for _ in range(5)]
    for i, p in enumerate(layout):
        for c in p:
            y, x = posof(c)
            m[y][x] = i + 1
    return m

class Node:
    def __init__(self, l, p = None):
        self.layout = l
        self.parent = p

def solve(start, end):
    visit = {frozenset(start)}
    queue = deque([Node(start)])
    while queue:
        cur = queue.popleft()
        if cur.layout[-1] == end:
            return cur
        else:
            lys = expand(cur.layout, visit)
            for ly in lys:
                queue.append(Node(ly, cur))
                visit.add(frozenset(ly))
    return None # no solution

def expand(layout, visit):
    def bound(piece, d):
        for c in piece:
            if c + d < 0 or c + d >= 20:
                return False
            if d == 1 and c % 4 == 3:
                return False
            if d == -1 and c % 4 == 0:
                return False
        return True

    m = matrix(layout)
    def valid(piece, d, i):
        for c in piece:
            y, x = posof(c + d)
            if m[y][x] not in [0, i + 1]:
                return False
        return True

    def unique(ly):
        n = frozenset(ly)
        m = frozenset(frozenset(mirror(c) for c in p) for p in ly)
        return (n not in visit) and (m not in visit)

    s = []
    for i, p in enumerate(layout):
        for d in [-1, 1, -4, 4]:
            if bound(p, d) and valid(p, d, i):
                ly = move(layout, i, d)
                if unique(ly):
                    s.append(ly)
    return s

def move(layout, i, d):
    ly = [deepcopy(p) for p in layout]
    ly[i] = frozenset(c + d for c in layout[i])
    return ly

def print_layout(ly):
    for r in matrix(ly):
        print(["%X" % x for x in r])
    print("\n")

# pretty print
def output(node):
    seq = []
    while node is not None:
        seq = [node.layout] + seq
        node = node.parent
    for layout in seq:
        print_layout(layout)
    print("total", len(seq) - 1, "steps")

if __name__ == "__main__":
    output(solve(START, END))

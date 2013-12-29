#!/usr/bin/python

# maze.py
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


def solve(m, src, dst):
    stack = [[src]]
    s = []
    while stack != []:
        path = stack.pop()
        if path[-1] == dst:
            s.append(path)
        else:
            for p in adjacent(m, path[-1]):
                if not p in path:
                    stack.append(path + [p])
    return s

def adjacent(m, p):
    (x, y) = p
    ds = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    ps = []
    for (dx, dy) in ds:
        x1 = x + dx
        y1 = y + dy
        if 0 <= x1 and x1 < len(m[0]) and 0 <= y1 and y1 < len(m) and m[y][x] == 0:
            ps.append((x1, y1))
    return ps

def test():
    mz = [[0, 0, 1, 0, 1, 1],
          [1, 0, 1, 0, 1, 1],
          [1, 0, 0, 0, 0, 0],
          [1, 1, 0, 1, 1, 1],
          [0, 0, 0, 0, 0, 0],
          [0, 0, 0, 1, 1, 0]]
    mz1 = [[0, 0, 0, 0, 0, 1],
           [1, 0, 1, 1, 0, 1],
           [1, 0, 1, 1, 0, 1],
           [1, 0, 1, 1, 0, 1],
           [1, 0, 0, 0, 0, 0],
           [1, 1, 1, 1, 1, 0]]
    for m in [mz, mz1]:
        print(solve(m, (0, 0), (5,5)))

if __name__ == "__main__":
    test()

# leapfrog2d.py
# Copyright (C) 2015 Liu Xinyu (liuxinyu95@gmail.com)
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

# Problem reference:
# Mathematical puzzles of Sam Loyd, selected and edited by Martin Gardner. 1959 by Dover Publications, Inc.
# Chinese translation:
# Chen Weipeng,
# Shanghai scientific & technological education publish house. 1999
# ISBN 7-5428-1928-3

# board layout:

# B, B, B
# B, B, B
# B, B, *, W, W
#       W, W, W
#       W, W, W

# The board is combined by two 3x3 squares. There are 16 pieces (8 black and 8 white)
# and an empty cell.
# where B is black piece, W is white piece, * is empty cell.
# rules: leap or hop to exchange all the blacks and whites
# the movement can only happen either vertically or horizontally, but not in diagonal direction.

# BFS solution

from collections import deque

def solve(i, j, start, end):
    visit = set([normalize(start)])
    queue = deque([[((i, j), start)]])
    while queue:
        c = queue.popleft()
        if snd(c[0]) == end:
            return [snd(_) for _ in c][::-1] # stop after finding the 1st solution
        else:
            for m in moves(c[0]):
                layout = normalize(snd(m))
                if layout not in visit:
                    queue.append([m] + c)
                    visit.add(layout)
    return None

DELTA = [(0, 1), (1, 0), (0, -1), (-1, 0)]
def moves(s):
    ((i, j), m) = s
    return [((i+di, j+dj), swap(m, i, j, di, dj))
            for (di, dj) in (DELTA + [(dx*2, dy*2) for (dx, dy) in DELTA])
            if 0 <= i + di and i + di < 5 and 0 <= j + dj and j + dj < 5 and
               m[i+di][j+dj] == - (di+dj) / abs(di + dj)]

def swap(m, i, j, di, dj):
    a = [_[:] for _ in m]
    (a[i+di][j+dj], a[i][j]) = (a[i][j], a[i+di][j+dj])
    return a

def snd(p):
    (_, b) = p
    return b

def normalize(m):
    return tuple([tuple(r) for r in m])

CMAP = {0:"*", 2:" ", 1:"B", -1:"W"}
def test():
    start = [[1,  1,  1,  2,  2],
             [1,  1,  1,  2,  2],
             [1,  1,  0, -1, -1],
             [2,  2, -1, -1, -1],
             [2,  2, -1, -1, -1]]
    end = [[-x if abs(x) < 2 else x for x in r] for r in start]
    s = solve(2, 2, start, end)
    for m in s:
        print "\n".join([" ".join([CMAP[x] for x in r]) for r in m]), "\n"
    print "total", len(s) - 1, "steps" # best solution: 46 steps


if __name__ == "__main__":
    test()

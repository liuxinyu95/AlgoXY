#!/usr/bin/python

# `Heng Dao Li Ma' layout
# 1 A A 2
# 1 A A 3
# 3 4 4 5
# 3 7 8 5
# 6 0 0 9

START = [[(1, 1), (2, 1)],
         [(1, 4), (2, 4)],
         [(3, 1), (4, 1)],
         [(3, 2), (3, 3)],
         [(3, 4), (4, 4)],
         [(5, 1)], [(4, 2)], [(4, 3)], [(5, 4)],
         [(1, 2), (1, 3), (2, 2), (2, 3)]]

def slove(start):
    visit = [sorted(start)]
    queue = [(start, [])]
    while queue != []:
        (cur, seq) = queue.pop(0)
        if cur[-1] == [(4, 2), (4, 3), (5, 2), (5, 3)]:
            return seq[::-1] # reversed(seq)
        else:
            for delta in expand(cur, visit):
                queue.append((move(cur, delta), [delta]+seq))
    return [] # no solution

def dup(layout):
    return [x[::1] for x in layout]

def move(layout, delta):
    (i, (dy, dx)) = delta
    m = dup(layout)
    m[i] = [(y + dy, x + dx) for (x, y) in m[i]]
    return m

def expand(layout, visit):
    def inBounds(p):
        return (1, 1) <= p and p <= (5, 4)
    def valid(i, y, x)
    s = []
    d = [(0, -1), (0, 1), (-1, 0), (1, 0)]
    for i in range(1, 11):
        for (dy, dx) in d:
            for (y, x) in layout[i]:
                (y1, x1) = (y + dy, x + dx)

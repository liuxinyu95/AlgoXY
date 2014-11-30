#!/usr/bin/python

from collections import deque

def solve():
    s0 = range(8)
    q = deque([(s0, None)])
    visit = {tuple(s0)}
    while q:
        (n, _) = s = q.pop()
        if n == [0, 7, 6, 5, 4, 3, 2, 1]:
            return backtrack(s)
        else:
            q.extendleft(slide(s, visit))
    return None # no solution

def backtrack(s):
    r = []
    (n, p) = s
    while p is not None:
        r.append(n)
        (n, p) = p
    return [n] + r[::-1]

def slide(s, visit):
    (n, _) = s
    cs = []
    for i in [left(n), right(n), up(n), down(n)]:
        if i != [] and (tuple(i) not in visit):
            visit.add(tuple(i))
            cs.append((i, s))
    return cs

def right(n):
    m = n[:]
    i = m.index(0)
    (m[i], m[i-1]) = (m[i-1], m[i])
    return m

def left(n):
    m = n[:]
    i = m.index(0)
    (m[i], m[i-7]) = (m[i-7], m[i])
    return m

def up(n):
    return ([0] + n[1:4] + [n[0]] + n[5:]) if n[4] == 0 else []

def down(n):
    return ([n[4]] + n[1:4] + [0] + n[5:]) if n[0] == 0 else []

if __name__ == "__main__":
    print(solve())

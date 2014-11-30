#!/usr/bin/python

from collections import deque

# BFS solution to Conway slide puzzle
# A set is introduced to record the visited states to avoid duplicated attempts
# A queue is used to control the search. The element in the queue is a pair, the
# state and its parent.

# The state of the sliding puzzle is a list of number. Let the initial state
#
# 1 ---- [ ] -----7
# |       |       |
# 2       |       6
# |       |       |
# 3 ----- 4 ------5
#
# be [0, 1, 2, 3, 4, 5, 6, 7]
# Where 0 represents the free cell. The goal is to transform to final state
# [0, 7, 6, 5, 4, 3, 2, 1].
#
# The rules is to slid any number to free cell as defined as left, right, up, down
# as below.
#
# When arrive at the final state, the program backtracks along with the parent state
# till the initial one. The result is a list of states from the start to the end.

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

# Slide possible tiles to the free cell
#   input the current state n, and the visited state history.
#   Try all movements and filter the duplicated ones by looking up the history set.
#   output a list of candates in (state, parent) form.

def slide(s, visit):
    (n, _) = s
    cs = []
    for i in [left(n), right(n), up(n), down(n)]:
        if i != [] and (tuple(i) not in visit):
            visit.add(tuple(i))
            cs.append((i, s))
    return cs

# xxxa0xxx -> xxx0axxx
def right(n):
    m = n[:]
    i = m.index(0)
    (m[i], m[i-1]) = (m[i-1], m[i])
    return m

# xxx0axxx -> xxxa0xxx
def left(n):
    m = n[:]
    i = m.index(0)
    (m[i], m[i-7]) = (m[i-7], m[i])
    return m

# axxx0xxx -> 0xxxaxxx
def up(n):
    return ([0] + n[1:4] + [n[0]] + n[5:]) if n[4] == 0 else []

# 0xxxaxxx -> axxx0xxx
def down(n):
    return ([n[4]] + n[1:4] + [0] + n[5:]) if n[0] == 0 else []

if __name__ == "__main__":
    print(solve())

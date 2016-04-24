#!/usr/bin/python

# 2D max sum
# Given a m*n matrix, find the sub matrix with maximum sum of its elements.

from random import *

# 1d sub-routine
def maxsum1(xs):
    s = 0 # max so far
    m = 0 # max end here
    for x in xs:
        m = max(m + x, 0)
        s = max(m, s)
    return s

# 2D solution
def maxsum2(m):
    n, k = len(m), len(m[0]) # number of row, col
    maxs = 0 # max so far
    for i in range(n):
        xs = [0] * k
        for j in range(i, n):
            xs = [x + y for (x, y) in zip(xs, m[j])]
            maxs = max(maxs, maxsum1(xs))
    return maxs

# naive 2D version
def naivemax(m):
    r, c = len(m), len(m[0])
    maxs = 0
    for left in range(c):
        for right in range(left, c):
            for up in range(r):
                for down in range(up, r):
                    maxs = max(maxs, sum([sum(row[left:right+1]) for row in m[up:down+1]]))
    return maxs

def test():
    M = 20
    for _ in range(10):
        row, col = randint(2, M), randint(2, M)
        m = [[randint(-M, M) for _ in range(col)] for _ in range(row)]
        assert maxsum2(m) == naivemax(m)

def example():
    m = [[1, 2, -1, -4, -20],
         [-8, -3, 4, 2, 1],
         [3, 8, 10, 1, 3],
         [-4, -1, 1, 7, -6]]
    print m
    print maxsum2(m) # should be sum2([[-3, 4, 2], [8, 10, 1], [-1, 1, 7]]) = 29
    print "naive"
    print naivemax(m)

if __name__ == "__main__":
    example()
    test()

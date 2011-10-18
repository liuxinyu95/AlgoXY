#!/usr/bin/python

import random

# DP solution based on [1].

class qtab:
    def __init__(self, n, low, up):
        self.q = [[False]*(up-low+1) for x in xrange(n)] # this is tricky!!![[False]*(up-low+1)]*n
        self.l = low
        self.u = up

    def set(self, i, j, x):
        self.q[i][j-self.l] = x

    def get(self, i, j):
        if j<self.l or j>self.u:
            return False
        return self.q[i][j-self.l]

    def output(self):
        for i in xrange(0, len(self.q)):
            for j in xrange(self.l, self.u+1):
                if self.q[i][j-self.l]:
                    print "(", i, j, ")",
            print ""

def solve(xs, s):
    low = sum([x for x in xs if x < 0])
    up  = sum([x for x in xs if x > 0])
    if s < low or s > up:
        return False
    n = len(xs)
    q = qtab(n, low, up)
    for i in xrange(0, n):
        for j in xrange(low, up+1):
            if i == 0:
                q.set(i, j, xs[i] == j)
            else:
                q.set(i, j, q.get(i-1, j) or xs[i]==j or q.get(i-1, j-xs[i]))
    return q.get(n-1, s)

def brute_force(xs, s):
    if len(xs)==1:
        return xs[0] == s
    else:
        return brute_force(xs[1:], s) or xs[0]==s or brute_force(xs[1:], s-xs[0])

def test():
    for i in xrange(100):
        n = random.randint(1, 10)
        xs = random.sample(xrange(-100, 100), n)
        l = sum([x for x in xs if x<0])
        u = sum([x for x in xs if x>0])
        s = random.randint(l, u)
        assert(brute_force(xs, s) == solve(xs, s))

if __name__ == "__main__":
    test()

# Reference
# [1]. http://en.wikipedia.org/wiki/Subset_sum_problem

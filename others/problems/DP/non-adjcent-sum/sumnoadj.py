#!/usr/bin/python

import random # for verification

# maximum sum of non-adjacent numbers.

# solution 1:
# Scan the list in one round, keep two variables i, e:
#   i: The maximum sum so far including current element
#   e: The maximum sum so far excluding current element
# Optimal sub structure
#   Scan the list from beginning, for any element xs[i], update i and e:
#   i' = xs[i] + e    include this element, and exclude the previous one;
#   e' = max(e, i)    skip this one, the maxium of previous i and e.
def maxsum(xs):
    i, e = 0, 0
    for x in xs:
        i, e = x + e, max(i, e)
    return max(i, e)

# solution 2: sub-set sum like DP solution
def maxsum2(xs):
    n = len(xs)
    if n <= 2:
        return 0 if not xs else max(xs)
    low = min(xs)
    up = sum(xs)
    if low == up:
        return low
    tab = [-1 for _ in xrange(up - low)] #-1: empty, else: the last selected index
    for i in xrange(n):
        tab[xs[i] - low] = i
    for i in xrange(n):
        for v in xrange(up - low):
            v1 = v - xs[i]
            if 0 <= v1 and v1 < up - low and tab[v1] not in [-1, i, i-1, i+1]:
                tab[v] = i
    for v in reversed(xrange(up - low)):
        if tab[v] != -1:
            return v + low
    return 0

N = 10

def test():
    for _ in xrange(N):
        n = random.randint(0, N)
        xs = [random.randint(0, N) for _ in xrange(n)]
        check(xs)

def check(xs):
    s, s1 = maxsum(xs), maxsum2(xs)
    if s1 != s:
        print "FAIL: expected:", s, "actual:", s1
        print xs
        exit()

if __name__ == "__main__":
    test()

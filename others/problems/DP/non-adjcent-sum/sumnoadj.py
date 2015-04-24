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

# variant of soluiton 1: folding
def maxsum1(xs):
    return max(reduce(lambda (i, e), n: (n + e, max(i, e)), xs, (0, 0)))

# solution 2: sub-set sum like DP solution
def maxsum2(xs):
    n = len(xs)
    m = 0 if not xs else max(xs)
    if m == 0 or n <= 2:
        return m;
    s = sum(xs)
    tab = [set() for _ in xrange(s)] #record the indices for the subset
    for i in xrange(n):
        tab[xs[i]].add(tuple([i]))
    for i in xrange(n):
        for v in xrange(s):
            v1 = v - xs[i]
            if 0 <= v1 and tab[v1]:
                tab[v] = tab[v] | fromlist([tuple(sorted([i] + list(t))) for t in tab[v1] if not adjacent(i, t)])
    for v in reversed(xrange(s)):
        if tab[v]:
            #print tab[v]
            return v
    return 0

def fromlist(xs):
    return set(tuple(sorted(xs)))

def adjacent(i, t):
    return i in t or (i-1) in t or (i+1) in t

N = 20  # as solution 2 is very slow, don't set N too big.

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

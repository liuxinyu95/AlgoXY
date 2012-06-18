#!/usr/bin/python

# Longest Common Subsequence, DP - memorization
import random

def lcs(xs, ys):
    m = len(xs)
    n = len(ys)
    c = [[0]*(n+1) for _ in xrange(m+1)] # Buggy if [[0]*(n+1)]*(m+1)
    for i in xrange(1, m+1):
        for j in xrange(1, n+1):
            if xs[i-1] == ys[j-1]:
                c[i][j] = c[i-1][j-1] + 1
            else:
                c[i][j] = max(c[i-1][j], c[i][j-1])

    return get(c, xs, ys, m, n)

# Alternatively, we can use a direction table along with C to record
# the result trace. Such table contains value such as ('NW', 'N', 'W').
def get(c, xs, ys, i, j):
    if i==0 or j==0:
        return []
    elif xs[i-1] == ys[j-1]:
        return get(c, xs, ys, i-1, j-1) + [xs[i-1]]
    elif c[i-1][j] > c[i][j-1]:
        return get(c, xs, ys, i-1, j)
    else:
        return get(c, xs, ys, i, j-1)

# the naive one is very slow, don't test it with big lists.
def naive_lcs(xs, ys):
    if xs == [] or ys == []:
        return []
    if xs[0] == ys[0]:
        return [xs[0]] + naive_lcs(xs[1:], ys[1:])
    else:
        a = naive_lcs(xs, ys[1:])
        b = naive_lcs(xs[1:], ys)
        if len(a) < len(b):
            return b
        else:
            return a

def test():
    N = 10  #Caution! don't use big number, or naive method will too slow.
    for i in xrange(100):
        m = random.randint(1, N)
        n = random.randint(1, N)
        xs = random.sample(xrange(N), m)
        ys = random.sample(xrange(N), n)
        #Note that two lcs algorithms may return different results with same length.
        assert(len(naive_lcs(xs, ys))==len(lcs(xs, ys)))
        print "test", i, "OK." 

if __name__ == "__main__":
    test()

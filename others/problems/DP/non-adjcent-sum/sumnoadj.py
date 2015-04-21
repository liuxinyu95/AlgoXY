#!/usr/bin/python

# maximum sum of non-adjacent numbers.

def maxsum(xs):
    if xs == []:
        return 0
    low = max(xs)
    up = sum(xs)
    print "low=", low, "up=", up, "xs=", xs
    t = [[] for _ in xrange(up - low)]
    print "t=", t
    for i in xrange(len(xs)):
        print "i=", i, "xs[i]=", xs[i]
        t[xs[i] - low] = [[i]]
    for i in xrange(len(xs)):
        tab = t[:]
        for v in xrange(up - low):
            v1 = v - xs[i]
            if 0 <= v1 and v1 < up - low and tab[v1] != []:
                tab[v] = tab[v] + [[i] + ys for ys in tab[v1] if not adjacent(i, tab[v1])]
        t = tab
    for v in reversed(xrange(up - low)):
        if tab[v] != []:
            return sum([xs[i] for i in tab[v]])
    return 0 #shouldn't be here

def adjacent(i, ns):
    return i-1 in ns or (i+1) in ns

def test():
    check([], 0)
    check([1], 1)
    check([1, 2], 2)
    check([1, 2, 3], 4)
    check([1, 2, 3, 4], 6)

def check(xs, s):
    s1 = maxsum(xs)
    if s1 != s:
        print "FAIL: expected:", s, "actual:", s1
        print xs
        exit()

if __name__ == "__main__":
    test()

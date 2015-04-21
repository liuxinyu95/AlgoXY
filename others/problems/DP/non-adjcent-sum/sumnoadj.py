#!/usr/bin/python

# maximum sum of non-adjacent numbers.

def maxsum(xs):
    low = max(xs)
    up = sum(xs)
    t = [[] for _ in xrange(low, up)]
    for i in xrange(len(xs)):
        t[x[i]].append([i])
    for i in xrange(len(xs)):
        tab = t[:]
        for v in xrange(low, up):
            v1 = v - x[i]
            if low <= v1 and v1 < up and tab[v1] != []:
                tab[v] = tab[v] + [[i] + ys for ys in tab[v1] if not adjacent(i, tab[v1])]
        t = tab
    for v in reversed(xrange(low, up)):
        if tab[v] != []:
            return sum([xs[i] for i in tab[v]])
    return 0 #shouldn't be here

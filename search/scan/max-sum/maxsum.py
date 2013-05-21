#!/usr/bin/python

# invariant
# .... max so far .... max end here
def maxsum(xs):
    s = 0 # max so far
    m = 0 # max end here
    for x in xs:
        m = max(m + x, 0)
        s = max(m, s)

def naive_maxsum(xs):
    pass

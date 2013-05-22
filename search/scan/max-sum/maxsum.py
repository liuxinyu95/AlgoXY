#!/usr/bin/python

# refer to column 7 in [1]
# [1]. Jon Bentley. ``Programming pearls, Second Edition''. Addison-Wesley Professional; 1999. ISBN-13: 978-0201657883

import random

# invariant
# .... max so far .... max end here
def maxsum(xs):
    s = 0 # max so far
    m = 0 # max end here
    for x in xs:
        m = max(m + x, 0)
        s = max(m, s)
    return s

def naive_maxsum(xs):
    n = len(xs)
    m = 0
    r = (0, 0)
    for i in range(n):
        s = 0
        for j in range(i, n):
            s = s + xs[j]
            if m < s:
                m = s
                r = (i, j)
    return (m, r)

def test():
    for _ in range(100):
        xs = random.sample(range(-100, 100), 100)
        (m, _) = naive_maxsum(xs)
        assert m == maxsum(xs)

def example():
    xs = [3, -13, 19, -12, 1, 9, 18, -16, 15, -15]
    print naive_maxsum(xs)

if __name__ == "__main__":
    example()
    test()

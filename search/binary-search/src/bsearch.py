#!/usr/bin/python

from random import *

def bsearch(xs, x):
    l = 0
    u = len(xs)
    while(l < u):
        m = (l+u)/2 #error if integer are in limited words, l + (u-l)/2
        if xs[m] == x:
            return m
        elif xs[m] < x:
            l = m + 1
        else:
            u = m
    return -1

def test():
    assert(bsearch([], 1) == -1)
    assert(bsearch([2], 1) == -1)
    assert(bsearch([1], 1) == 0)
    #assert(bsearch([1,1,1], 1) == 0)
    assert(bsearch([1,2,3], -1) == -1)
    assert(bsearch([1,2,3], 5) == -1)
    assert(bsearch([3,4,7,9,11], 4) == 1)
    assert(bsearch([3,4,7,9,11], 9) == 3)
    assert(bsearch([3,4,7,9,11], 3) == 0)
    assert(bsearch([3,4,7,9,11], 11) == 4)

if __name__ == "__main__":
    test()

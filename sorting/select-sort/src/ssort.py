#!/usr/bin/python

import random

def naive_ssort(xs):
    ys = []
    while xs != []:
        m = min(xs)
        xs.remove(m)
        ys.append(m)
    return ys

def in_place_ssort(xs):
    n = len(xs)
    for i in range(n):
        m = min_at(xs, i, n)
        (xs[i], xs[m]) = (xs[m], xs[i])
    return xs

def min_at(xs, i, n):
    m = i;
    for j in range(i+1, n):
        if xs[j] < xs[m]:
            m = j
    return m

def test_sort(f, xs):
    assert sorted(xs) == f([y for y in xs])

def test():
    for _ in xrange(100):
        n = random.randint(0, 100)
        xs = random.sample(range(100), n)
        test_sort(naive_ssort, xs)
        test_sort(in_place_ssort, xs)

if __name__ == "__main__":
    test()


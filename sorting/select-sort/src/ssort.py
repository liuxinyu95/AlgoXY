#!/usr/bin/python

import random

def naive_ssort(xs):
    ys = []
    while xs != []:
        m = min(xs)
        xs.remove(m)
        ys.append(m)
    return ys

def test_sort(f, xs):
    assert sorted(xs) == f(xs)

def test_naive_sort():
    for _ in xrange(100):
        n = random.randint(0, 100)
        xs = random.sample(range(100), n)
        test_sort(naive_ssort, xs)

if __name__ == "__main__":
    test_naive_sort()

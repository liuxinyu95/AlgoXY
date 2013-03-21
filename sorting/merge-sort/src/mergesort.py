#!/usr/bin/python

import random

N = 10000

# 1. basic version as described in CLRS (Introduction to algorithm)
def msort(xs):
    n = len(xs)
    if n > 1:
        ys = [x for x in xs[:n/2]]
        zs = [x for x in xs[n/2:]]
        ys = msort(ys)
        zs = msort(zs)
        xs = merge(xs, ys, zs)
    return xs

# verbose version without using sentinel
# xs = ys `merge` zs
def merge(xs, ys, zs):
    i = 0
    while ys != [] and zs != []:
        xs[i] = ys.pop(0) if ys[0] < zs[0] else zs.pop(0)
        i = i + 1
    xs[i:] = ys if ys !=[] else zs
    return xs

def test_sort(fsort):
    for _ in range(100):
        xs = random.sample(range(N) * 10, random.randint(0, N))
        assert sorted(xs) == fsort(xs)

def test():
    test_sort(msort)

if __name__ == "__main__":
    test()

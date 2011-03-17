#!/usr/bin/python

import random

def partition_with(xs, l, u, x):
    left = l
    for right in range(l, u+1):
        if xs[right] <= x:
            (xs[left], xs[right]) = (xs[right], xs[left])
            left = left + 1
    return left

# Randomized partition with
def partition(xs, l, u):
    return partition_with(xs, l, u, xs[random.randint(l, u)])

# method 1, randomized partition
def partition_at(xs, k):
    (l, u)=(0, len(xs) - 1)
    while True:
        m = partition(xs, l, u)
        if m < k:
            l = m
        elif m > k:
            u = m - 1
        else:
            return

# method 2, min-max
def partition_at2(xs, k):
    (l, u) = (0, len(xs) - 1)
    while True:
        a = max_at(xs, l, k)
        b = min_at(xs, k, u)
        if xs[a] > xs[b]:
            (xs[a], xs[b]) = (xs[b], xs[a])
            (l, u) = (partition_with(xs, l, k-1, xs[a]), partition_with(xs, k, u, xs[b]) - 1)
        else:
            return

def max_at(xs, l, u):
    i = l
    for j in xrange(l, u+1):
        if xs[j] > xs[i]:
            i = j
    return i

def min_at(xs, l, u):
    i = l
    for j in xrange(l, u+1):
        if xs[j] < xs[i]:
            i = j
    return i

def verify(xs, k):
    assert(sorted(xs[:k]) == sorted(xs)[:k])

def gen_xs(n):
    xs = random.sample(range(n), random.randint(2, n))
    k = random.randint(1, len(xs)-1)
    return (xs, k)

def test():
    n = 10000
    for i in range(100):
        (xs, k)=gen_xs(n)
        partition_at(xs, k)
        verify(xs, k)
        (xs, k)=gen_xs(n)
        partition_at2(xs, k)
        verify(xs, k)

if __name__ == "__main__":
    test()
    

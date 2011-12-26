#!/usr/bin/python

# Counting Sort
# Based on Knuth, TAOCP Chapter 5

import random

def csort(xs):
    n = len(xs)
    cnt = [0]*n
    for i in range(n-1, 0, -1):
        for j in range(i-1, -1, -1):
            if xs[i] < xs[j]:
                cnt[j] = cnt[j] + 1
            else:
                cnt[i] = cnt[i] + 1
    for i in range(n):
        while cnt[i] != i:
            j = cnt[i]
            (xs[i], xs[j]) = (xs[j], xs[i])
            (cnt[i], cnt[j]) = (cnt[j], cnt[i])
    return xs

def test():
    for i in range(100):
        xs=[]
        n = random.randint(0, 1000)
        for j in range(n):
            xs.append(random.randint(-1000, 1000))
        assert(sorted(xs) == csort(xs))

if __name__ == "__main__":
    test()

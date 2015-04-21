#!/usr/bin/python

# Find the median of two sorted arrays.
# Recursive solution

# For the two arrays A and B:

# General recursive case:
#   Calculate the medians m1 and m2 of the input arrays A and B respectively.
#   If m1 < m2:
#      Recursive find the median of two sub-arrays: A[m1...] and B[...m2]
#   Otherwise, m2 < m1:
#      Recursive find the median of two sub-arrays: A[...m1] and B[m2...]

import random

def median(xs, ys):
    n, m = len(xs), len(ys)
    med = kth(xs, 0, n, ys, 0, m, (m + n) / 2 + 1)
    if (n + m) % 2 == 0:
        return (med + kth(xs, 0, n, ys, 0, m, (m + n) / 2)) / 2.0
    return med

# find the k-th element from xs[x0, x1) and ys[y0, y1), k starts from 1
def kth(xs, x0, x1, ys, y0, y1, k):
    if x1 - x0 < y1 - y0:
        return kth(ys, y0, y1, xs, x0, x1, k)
    if x1 <= x0:
        return ys[y0 + k - 1]
    if y1 <= y0:
        return xs[x0 + k - 1]
    if k == 1:
        return min(xs[x0], ys[y0])
    j = min(k/2, y1 - y0)
    i = k - j
    i, j = x0 + i, y0 + j
    if xs[i-1] < ys[j-1]:
        return kth(xs, i, x1, ys, y0, j, k - i + x0)
    else:
        return kth(xs, x0, i, ys, j, y1, k - j + y0)

N = 100

def test():
    for _ in range(N):
        xs = range(random.randint(1, N))
        m = (len(xs) - 1) / 2.0
        random.shuffle(xs)
        n = random.randint(0, len(xs))
        (xs, ys) = (sorted(xs[:n]), sorted(xs[n:]))
        m1 = median(xs, ys)
        if abs(m - m1) > 0.005:
            print "FAIL: median expected:", m, "actual:", m1
            print xs, "len=", len(xs)
            print ys, "len=", len(ys)
            exit()

if __name__ == "__main__":
    test()

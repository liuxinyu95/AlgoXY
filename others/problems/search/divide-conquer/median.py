#!/usr/bin/python

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
    print xs, "len=", n
    print ys, "len=", m
    med = kth(xs, 0, n, ys, 0, m, (m + n) / 2 + 1)
    if (n + m) % 2 == 0:
        return (med + kth(xs, 0, n, ys, 0, m, (m + n) / 2)) / 2.0
    return med

# find the k-th element from xs[x0, x1) and ys[y0, y1), k starts from 1
def kth(xs, x0, x1, ys, y0, y1, k):
    print "xs=", xs, "(", x0, x1, ")\nys=", ys, "(", y0, y1, ")\nk=", k,
    if x1 - x0 < y1 - y0:
        return kth(ys, y0, y1, xs, x0, x1, k)
    if x1 <= x0:
        print "found: ys[y0+k]=", ys[y0+k-1]
        return ys[y0 + k - 1]
    if y1 <= y0:
        print "found: xs[x0+k]=", xs[x0+k-1]
        return xs[x0 + k - 1]
    if k == 1:
        print "found: min(xs[0], ys[0])=", min(xs[x0], ys[y0])
        return min(xs[x0], ys[y0])
    j = min(k/2, y1 - y0)
    i = k - j
    i, j = x0 + i, y0 + j
    if xs[i-1] < ys[j-1]:
        print "too small"
        return kth(xs, i, x1, ys, y0, j, k - i)
    else:
        print "too big"
        return kth(xs, x0, i, ys, j, y1, k - i)

N = 20 #100

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
        else:
            print "\n"

if __name__ == "__main__":
    test()

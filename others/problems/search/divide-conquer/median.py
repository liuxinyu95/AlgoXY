#!/usr/bin/python

# median.py
# Copyright (C) 2015 Liu Xinyu (liuxinyu95@gmail.com)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.



# Find the meidan of two sorted arrays
# For arrays, A of length m, and B of length n (index starts from 1)
# Find the median by using the k-th element method.
# Where
#    median = k-th(A, B, k), k = floor(m + n) / 2 + 1) if m + n is odd
#    median = (k-th(A, B, k) + k-th(A, B, k-1)) / 2 if m + n is even

# In order to find the k-th element in A, and B
# Suppose m >= n, (exchange A, B otherwise)
# If B is empty, return the k-th element in A;
# If k = 1, return the minimum of A[1] and B[1]
# Otherwise, guess j = min(k/2, n), and i = k - j
# then compare A[i] and B[j]
# If A[i] < B[j], Drop all elements before A[i] and after B[j], then
# recursively find the (k - i)-th element in the rest.
# Othewise, Drop all elements before B[j] and after A[i], then recursively
# find the (k-j)-th element in the rest.

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

#!/usr/bin/python

# lcs.py
# Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
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


# Longest Common Subsequence, DP - memorization
import random

def lcs(xs, ys):
    m, n = len(xs), len(ys)
    c = [[0]*(n + 1) for _ in range(m + 1)]
    for i in range(1, m+1):
        for j in range(1, n+1):
            if xs[i-1] == ys[j-1]:
                c[i][j] = c[i-1][j-1] + 1
            else:
                c[i][j] = max(c[i-1][j], c[i][j-1])
    return fetch(c, xs, ys)

def fetch(c, xs, ys):
    r = []
    m, n = len(xs), len(ys)
    while m > 0 and n > 0:
        if xs[m - 1] == ys[n - 1]:
            r.append(xs[m - 1])
            m = m - 1
            n = n - 1
        elif c[m - 1][n] > c[m][n - 1]:
            m = m - 1
        else:
            n = n - 1
    r.reverse()
    return r

# the naive one is very slow, don't test it with big lists.
def naive_lcs(xs, ys):
    if xs == [] or ys == []:
        return []
    if xs[0] == ys[0]:
        return [xs[0]] + naive_lcs(xs[1:], ys[1:])
    else:
        a = naive_lcs(xs, ys[1:])
        b = naive_lcs(xs[1:], ys)
        if len(a) < len(b):
            return b
        else:
            return a

def test():
    N = 10  #Caution! don't use big number, or naive method will too slow.
    for i in range(100):
        m = random.randint(1, N)
        n = random.randint(1, N)
        xs = random.sample(range(N), m)
        ys = random.sample(range(N), n)
        #Note that two lcs algorithms may return different results with same length.
        assert(len(naive_lcs(xs, ys))==len(lcs(xs, ys)))
        print("test", i, "OK.")

if __name__ == "__main__":
    test()

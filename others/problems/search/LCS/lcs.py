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
from enum import Flag, auto

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

# Record both length and direction.
class Dir(Flag):
    N = auto()
    W = auto()
    NW = auto()

def lcs_dir(xs, ys):
    fst = lambda p : p[0]
    m, n = len(xs), len(ys)
    c = [[(0, None)]*(n + 1) for _ in range(m + 1)]
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if xs[i-1] == ys[j-1]:
                c[i][j] = (c[i-1][j-1][0] + 1, Dir.NW)
            else:
                c[i][j] = (max(c[i-1][j], c[i][j-1], key=fst)[0], \
                           Dir.N if fst(c[i-1][j]) > fst(c[i][j-1]) else Dir.W)
    return fetch_dir(c, xs, ys)

def fetch_dir(c, xs, ys):
    snd = lambda p : p[1]
    r = []
    m, n = len(xs), len(ys)
    while m > 0 and n > 0:
        d = snd(c[m][n])
        if d == Dir.NW:
            r.append(xs[m - 1]) # or ys[n - 1]
            m = m - 1
            n = n - 1
        elif d == Dir.N:
            m = m - 1
        elif d == Dir.W:
            n = n - 1
        else:
            assert False, f"should not be here {d}"
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

# Caution! don't use big num for the naive method.
def test(lcs_func, num = 10):
    for i in range(100):
        m = random.randint(1, num)
        n = random.randint(1, num)
        xs = random.sample(range(num), m)
        ys = random.sample(range(num), n)
        #Note that two lcs algorithms may return different results with the same length.
        assert len(naive_lcs(xs, ys)) == len(lcs_func(xs, ys)),  "fail"
    print("test", lcs_func, "with 100 cases OK.")

if __name__ == "__main__":
    test(lcs)
    test(lcs_dir)

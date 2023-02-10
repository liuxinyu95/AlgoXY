#!/usr/bin/python

# subsetsum.py
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


import random # for verification purpose only

# A brute-force solution only answers the existence of subset for a given sum.
def brute_force(xs, s):
    if xs == []:
        return s == 0
    if len(xs) == 1:
        return xs[0] == s
    else:
        return brute_force(xs[1:], s) or xs[0]==s or brute_force(xs[1:], s-xs[0])

# Method 1, DP solution based on [1].
def solve(xs, s):
    low = sum([x for x in xs if x < 0])
    up  = sum([x for x in xs if x > 0])
    def col(j):
        return j - low
    n = len(xs)
    tab = [[False]*(up - low + 1) for _ in range(n + 1)]
    tab[0][col(0)] = True
    for i, x in enumerate(xs, start = 1):
        tab[i][col(x)] = True
        for j in range(low, up + 1):
            tab[i][col(j)] = tab[i][col(j)] or tab[i-1][col(j)]
            j1 = j - x
            if low <= j1 and j1 <= up:
                tab[i][col(j)] = tab[i][col(j)] or tab[i-1][col(j1)]
    def fetch(s, i):
        r = []
        if xs[i - 1] == s:
            r.append([xs[i - 1]])
        if i > 0:
            if tab[i - 1][col(s)]:
                r = r + fetch(s, i - 1)
            s = s - xs[i - 1]
            if low <= s and s <= up and tab[i-1][col(s)]:
                r = r + [[xs[i - 1]] + ys for ys in fetch(s, i-1)]
        return r
    return fetch(s, n)

# Method 2: Use a vector instead of a 2D table.
def subsetsum(xs, s):
    low = sum([x for x in xs if x < 0])
    up  = sum([x for x in xs if x > 0])
    tab = [set([]) for _ in range(low, up+1)]
    for x in xs:
        tab1 = tab[:]
        for j in range(low, up+1):
            if x == j:
                tab1[j] = tab1[j] | {frozenset([x])}
            j1 = j - x
            if low <= j1 and j1 <= up and tab[j1]:
                tab1[j] = tab1[j] | frozenset(ys | frozenset([x]) for ys in tab[j1])
        tab = tab1
    return list(tab[s])

# Verification
def test():
    num = 100
    for i in range(num):
        n = random.randint(1, 10)
        xs = random.sample(range(-100, 100), n)
        l = sum([x for x in xs if x<0])
        u = sum([x for x in xs if x>0])
        s = random.randint(l, u)
        exist = brute_force(xs, s)
        s1 = solve(xs, s)
        s2 = subsetsum(xs, s)
        #print(s1, s2, s, xs)
        if exist:
            assert(s1 and all(sum(st) == s for st in (s1 + s2)) and len(s1) == len(s2))
        else:
            assert(s1 == [] and s2 == [])
    print(num, "test passed")

if __name__ == "__main__":
    test()
    #[-87, -38, -14] -101 ==> [] 0
    #print(solve([-3, -2, -1], -4))
    #print(subsetsum([-3, -2, -1], -4))

# Reference
# [1]. http://en.wikipedia.org/wiki/Subset_sum_problem

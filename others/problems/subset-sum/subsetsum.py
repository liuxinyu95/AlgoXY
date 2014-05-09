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


import random

# DP solution based on [1].
# Note the following solution only answers the existence of subset for a given value.

class qtab:
    def __init__(self, n, low, up):
        self.q = [[False]*(up-low+1) for x in xrange(n)] # this is tricky!!![[False]*(up-low+1)]*n
        self.l = low
        self.u = up

    def set(self, i, j, x):
        self.q[i][j-self.l] = x

    def get(self, i, j):
        if j<self.l or j>self.u:
            return False
        return self.q[i][j-self.l]

def solve(xs, s):
    low = sum([x for x in xs if x < 0])
    up  = sum([x for x in xs if x > 0])
    if s < low or s > up:
        return False
    n = len(xs)
    q = qtab(n, low, up)
    for i in xrange(0, n):
        for j in xrange(low, up+1):
            if i == 0:
                q.set(i, j, xs[i] == j)
            else:
                q.set(i, j, q.get(i-1, j) or xs[i]==j or q.get(i-1, j-xs[i]))
    return q.get(n-1, s)

def brute_force(xs, s):
    if len(xs)==1:
        return xs[0] == s
    else:
        return brute_force(xs[1:], s) or xs[0]==s or brute_force(xs[1:], s-xs[0])

# Give the detailed solution for subset sum, (not only output the existence result)
def subsetsum(xs, s):
    low = sum([x for x in xs if x < 0])
    up  = sum([x for x in xs if x > 0])
    tab = [[[] for _ in xrange(low, up+1)]]
    for x in xs:
        row = tab[-1][:]
        for j in xrange(low, up+1):
            if x == j:
                row[j-low].append([x])
            j1 = j - x
            if low <= j1 and j1 <= up and tab[-1][j1-low] != []:
                row[j-low] = row[j-low] + [[x] + ys for ys in tab[-1][j1-low]]
        tab.append(row)
    return tab[-1][s-low]

# TODO: save space

def test():
    for i in xrange(100):
        n = random.randint(1, 10)
        xs = random.sample(xrange(-100, 100), n)
        l = sum([x for x in xs if x<0])
        u = sum([x for x in xs if x>0])
        s = random.randint(l, u)
        exist = brute_force(xs, s)
        assert( exist == solve(xs, s))
        if exist:
            print xs, s, "==>", subsetsum(xs, s)

if __name__ == "__main__":
    test()

# Reference
# [1]. http://en.wikipedia.org/wiki/Subset_sum_problem

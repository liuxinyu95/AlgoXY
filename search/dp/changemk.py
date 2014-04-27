#!/usr/bin/python

# changemk.py
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

# [1] ``Dynamic Programming Solution to the Coin Changing Problem''.
#   2004, open course. CSG 713 Advanced algorithms
# http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf

# method 1, top-down recursive solution with table

tab = [[] for _ in range(1000)]

def change(x, cs):
    if x > 0 and tab[x] == []:
        for s in [[c] + change(x - c, cs) for c in cs if c <= x]:
            if tab[x] == [] or len(s) < len(tab[x]):
                tab[x] = s
    return tab[x]

# method 2, bottom-up DP solution

def changemk(x, cs):
    s = [[] for _ in range(x+1)]
    for i in range(1, x+1):
        for c in cs:
            if c <= i and (s[i] == [] or 1 + len(s[i-c]) < len(s[i])):
                s[i] = [c] + s[i-c]
    return s[x]

#method 3, bottom-up DP solution, with fewer space based on [1]

def chgmk(x, cs):
    cnt = [0] + [x+1] * x
    s = [0]
    for i in range(1, x+1):
        coin = 0
        for c in cs:
            if c <= i and 1 + cnt[i-c] < cnt[i]:
                cnt[i] = 1 + cnt[i-c]
                coin = c
        s.append(coin)
    r = []
    while x > 0:
        r.append(s[x])
        x = x - s[x]
    return r

# Examples

USA = [1, 5, 25, 50, 100]

def test():
    print(change(142, USA))
    for i in range(1000):
        tab[i] = []
    print(change(6, [1, 2, 4]))

def test1():
    print(changemk(142, USA))
    print(changemk(6, [1, 2, 4]))

def test2():
    print(chgmk(142, USA))
    print(chgmk(6, [1, 2, 4]))

if __name__ == "__main__":
    test()
    test1()
    test2()

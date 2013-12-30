#!/usr/bin/python

# leapfrog.py
# Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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


def solve(start, end):
    stack = [[start]]
    s = []
    while stack != []:
        c = stack.pop()
        if c[0] == end:
            s.append(reverse(c))
        else:
            for m in moves(c[0]):
                stack.append([m]+c)
    return s

def moves(s):
    ms = []
    n = len(s)
    p = s.index(0)
    if p < n - 2 and s[p+2] > 0:
        ms.append(swap(s, p, p+2))
    if p < n - 1 and s[p+1] > 0:
        ms.append(swap(s, p, p+1))
    if p > 1 and s[p-2] < 0:
        ms.append(swap(s, p, p-2))
    if p > 0 and s[p-1] < 0:
        ms.append(swap(s, p, p-1))
    return ms

def swap(s, i, j):
    a = s[:]  # create a shallow copy by slice
    (a[i], a[j]) = (a[j], a[i])
    return a

# the built-in reversed() function returns a
# list-reverse-iterator, but not a list, which can't be
# applied directly to len() etc.
def reverse(x):
    return x[::-1] # reverse by slice

def test():
    for i in range(1, 7):
        start = [-1]*i + [0] + [1]*i
        output(solve(start, [-x for x in start]))

def output(s):
    for m in s:
        print m
        print "total", len(m) - 1, "steps"
    print "total", len(s), "solutions"

if __name__ == "__main__":
    test()

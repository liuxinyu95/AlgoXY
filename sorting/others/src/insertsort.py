#!/usr/bin/python

# insertsort.py
# Copyright (C) 2012 Liu Xinyu (liuxinyu95@gmail.com)
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

import random #for verification purpose only

# Insertion sort

# standard version.
def isort(xs):
    n = len(xs)
    for i in range(1, n):
        # insert xs[i] to xs[0..i-1]
        x = xs[i]
        j = i - 1
        while j >= 0 and x < xs[j]:
            xs[j+1] = xs[j]
            j = j - 1
        xs[j+1] = x
    return xs

# verbose version
def isort1(xs):
    ys = []
    for x in xs:
        insert(ys, x)
    return ys

def insert(xs, x):
    xs.append(x)
    i = len(xs) - 1
    while i>0 and xs[i] < xs[i-1]:
        xs[i], xs[i-1] = xs[i-1], xs[i] #swap
        i = i - 1

# improvement 1
#  Using binary search for insertion, 
#    Compare O(N * lg N)
#    Move O(N^2)

def isort2(xs):
    n = len(xs)
    for i in range(1, n):
        # insert xs[i] to xs[0..i-1]
        x = xs[i]
        p = binary_search(xs[:i], x)
        for j in range(i, p, -1):
            xs[j] = xs[j-1]
        xs[p] = x
    return xs

# modified binary search, x may not exist in xs
def binary_search(xs, x):
    l = 0
    u = len(xs)
    while l < u:
        m = (l+u)/2
        if xs[m] == x:
            return m # find a duplicated element
        elif xs[m] < x:
            l = m + 1
        else:
            u = m
    return l

# improvement 2,
#   Using linked-list to gain constant time, O(1), insert operation.
#   Instead of using pointer based linked-list, we can use
#   index based linked-list

def isort3(xs):
    n = len(xs)
    next = [-1]*(n+1)
    for i in range(n):
        insert1(xs, next, i)
    return reorder(xs, next)

def insert1(xs, next, i):
    j = -1
    while next[j] != -1 and xs[next[j]] < xs[i]:
        j = next[j]
    next[j], next[i] = i, next[j]

def reorder(xs, next):
    i = -1
    ys = []
    while next[i] != -1:
        ys.append(xs[next[i]])
        i = next[i]
    return ys

def test():
    for _ in xrange(100):
        n = random.randint(0, 100)
        xs = [random.randint(0, n) for _ in range(n)]
        ys = sorted(xs)
        assert(isort(list(xs)) == ys)
        assert(isort1(list(xs)) == ys)
        assert(isort2(list(xs)) == ys)
        assert(isort3(list(xs)) == ys)

# internal use only, need remove
def assertEqual(xs, ys):
    if not xs == ys:
        print "xs", xs
        print "ys", ys
        exit(1)
    
if __name__ == "__main__":
    #print isort3([5,1,2,4,3,3])
    test()

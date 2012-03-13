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

# As insertion sort is O(N^2), we limited the number of N
# to save time
def test():
    for _ in xrange(100):
        n = random.randint(0, 100)
        xs = [random.randint(0, n) for _ in range(n)]
        assert(isort(list(xs)) == sorted(xs))
        assert(isort1(list(xs)) == sorted(xs))

# internal use only, need remove
def assertEqual(xs, ys):
    if not xs == ys:
        print "xs", xs
        print "ys", ys
        exit(1)
    
if __name__ == "__main__":
    print isort1([5,1,2,4,3,3])
    test()

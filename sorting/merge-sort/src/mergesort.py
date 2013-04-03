#!/usr/bin/python

# mergesort.py
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


import random

N = 20 #10000

# 1. basic version as described in CLRS (Introduction to algorithm)
def msort(xs):
    n = len(xs)
    if n > 1:
        ys = [x for x in xs[:n/2]]
        zs = [x for x in xs[n/2:]]
        ys = msort(ys)
        zs = msort(zs)
        xs = merge(xs, ys, zs)
    return xs

# verbose version without using sentinel
# xs = ys `merge` zs
def merge(xs, ys, zs):
    i = 0
    while ys != [] and zs != []:
        xs[i] = ys.pop(0) if ys[0] < zs[0] else zs.pop(0)
        i = i + 1
    xs[i:] = ys if ys !=[] else zs
    return xs

# Bottom-up merge sort version.
def mergesort(xs):
    ys = [x for x in xs]
    n = len(xs)
    i = 1
    while i < n:
        for j in range(0, n, i):
            print "merge", xs[j:j+i], xs[j+i:j+2*i]
            ys[j:j+2*i] = merge(ys[j:j+2*i], xs[j:j+i], xs[j+i:j+2*i])
            print "merged", ys[j:j+2*i]
        print "i=", i, "ys=", ys
        xs = [y for y in ys]
        i = i * 2
    return xs

def test_sort(fsort):
    for _ in range(100):
        xs = random.sample(range(N) * 10, random.randint(0, N))
        #assert sorted(xs) == fsort(xs)
        ys = [x for x in xs]
        if sorted(xs) != fsort(xs):
            print "ys=", ys
            print "xs=", sorted(ys)
            print "zs=", fsort(ys)
            exit()

def test():
    print mergesort([7, 14, 10, 1, 1, 10, 16, 19, 17, 1])
    #test_sort(msort)
    test_sort(mergesort)

if __name__ == "__main__":
    test()

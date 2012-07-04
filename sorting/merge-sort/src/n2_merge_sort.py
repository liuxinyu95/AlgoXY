#!/usr/bin/python

# n2_merge_sort.py
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


# Nature 2-way merge sort, refer to [1]
# [1], Donald E Knuth, `The art of computer programming, Volume 3, sorting and searching'

import random

def sort(xs):
    n = len(xs)
    if n < 2:
        return xs
    ys = [x for x in xs]
    while True:
        (l, i, j, r) = (0, 1, n-1, n)
        while True:
            while i < j and xs[i] < xs[i+1]:
                i = i + 1
            while i < j and xs[j-1] < xs [j]:
                j = j - 1
            if i > j:
                i = j
            print i, j
            merge(xs[l:i], xs[j:r], ys, l + n - r)
            if i == j:
                break
            (l, i, j, r) = (i, i+1, j-1 , j)
        (xs, ys) = (ys, xs)
        if i >= n - 1:
            break
    return xs

def merge(xs, ys, zs, k):
    print xs, ys, zs, k
    while xs != [] and ys != []:
        if xs[0] < ys[0]:
            zs[k] = xs[0]
            xs = xs[1:]
        else:
            zs[k] = ys[0]
            ys = ys[1:]
        k = k + 1
    if xs != []:
        for x in xs:
            zs[k] = x
            k = k + 1
    if ys != []:
        for y in ys:
            zs[k] = y
            k = k + 1
    print "merge res:", zs

def __assert(xs0, xs, ys):
    if xs != ys:
        print "sort", xs0, "=>", xs, "!=", ys
        exit()

def verbose_test(xs):
    zs = [x for x in xs]
    ys = sorted(xs)
    xs = sort(xs)
    if xs != ys:
        print "sort", zs, "=>", xs, "!=", ys
        exit()
    

def test():
    
    verbose_test([])
    verbose_test([1])
    verbose_test([1, 2])
    verbose_test([2, 1])
    verbose_test([1, 3, 2])
    verbose_test([1, 3, 2, 4])

if __name__ == "__main__":
    test()

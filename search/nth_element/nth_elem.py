#!/usr/bin/python

# nth_elem.py
# Copyright (C) 2011 Liu Xinyu (liuxinyu95@gmail.com)
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
# 


import random

def partition_with(xs, l, u, x):
    left = l
    for right in range(l, u+1):
        if xs[right] <= x:
            (xs[left], xs[right]) = (xs[right], xs[left])
            left = left + 1
    return left

# Randomized partition with
def partition(xs, l, u):
    return partition_with(xs, l, u, xs[random.randint(l, u)])

# method 1, randomized partition
def partition_at(xs, k):
    (l, u)=(0, len(xs) - 1)
    while True:
        m = partition(xs, l, u)
        if m < k:
            l = m
        elif m > k:
            u = m - 1
        else:
            return

# method 2, min-max
def partition_at2(xs, k):
    (l, u) = (0, len(xs) - 1)
    while True:
        a = max_at(xs, l, k)
        b = min_at(xs, k, u)
        if xs[a] > xs[b]:
            (xs[a], xs[b]) = (xs[b], xs[a])
            (l, u) = (partition_with(xs, l, k-1, xs[a]), partition_with(xs, k, u, xs[b]) - 1)
        else:
            return

def max_at(xs, l, u):
    i = l
    for j in xrange(l, u+1):
        if xs[j] > xs[i]:
            i = j
    return i

def min_at(xs, l, u):
    i = l
    for j in xrange(l, u+1):
        if xs[j] < xs[i]:
            i = j
    return i

def verify(xs, k):
    assert(sorted(xs[:k]) == sorted(xs)[:k])

def gen_xs(n):
    xs = random.sample(range(n), random.randint(2, n))
    k = random.randint(1, len(xs)-1)
    return (xs, k)

def test():
    n = 10000
    for i in range(100):
        (xs, k)=gen_xs(n)
        partition_at(xs, k)
        verify(xs, k)
        (xs, k)=gen_xs(n)
        partition_at2(xs, k)
        verify(xs, k)

if __name__ == "__main__":
    test()
    

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
# [1], Donald E Knuth, `The art of computer programming, Volume 3, sorting and searching', 5.2.4

import random

#
#  sort xs in-plance
#
#  xs = xs[0], xs[1], ...xs[a], ..., xs[b], ... xs[c], ..., xs[d], ...xs[n-1]
#     range xs[a,b] is monotonic increase; 
#     range xs[c,d] is monotonic decrease;
#
#  ys = ys[0], ys[1], ...ys[front], ..., ..., ys[rear], .... ys[n-1]
#     range ys[0, front] is monotonic increase;
#     range ys[rear, n-1] is monotonic decrease;
#
#  Repeatedly merge xs[a, b] and xs[c, d] to ys[target], where target = front and rear in turn.
#  till b overlaps c, then repeat the whole process till xs[a, b] == xs
#

def sort(xs):
    if len(xs) < 2:
        return xs
    ordered = False
    while not ordered:
        (ordered, xs) = proc(xs)
    return xs

# take one pass
def proc(xs):
    n = len(xs)
    ys = [xs[0]]*n
    (a, b) = (0, 0)
    (c, d) = (n, n)
    (f, r) = (0, n-1)
    t = True # put result from left if true, else put result from right
    while b < c:
        # span [a, b) as much as possible
        while b < c:
            b = b + 1
            if b < n and xs[b] < xs[b-1]:
                break
        # span [c, d) as much as possible
        while b < c:
            c = c - 1
            if c < n and xs[c-1] < xs[c]:
                break
        if b - a == n:
            return (True, xs)
        if t:
            f = merge(xs, a, b, c, d, ys, f, 1)
        else:
            r = merge(xs, a, b, c, d, ys, r, -1)
        (a, d) = (b, c)
        t = not t
    return (False, ys)

def merge(xs, a, b, c, d, ys, k, delta):
    while a < b and c < d:
        if xs[a] < xs[d-1]:
            ys[k] = xs[a]
            a = a + 1
        else:
            ys[k] = xs[d-1]
            d = d - 1
        k = k + delta
    while a < b:
        ys[k] = xs[a]
        k = k + delta
        a = a + 1
    while c < d:
        ys[k] = xs[d-1]
        k = k + delta
        d = d - 1
    return k

def test():
    for i in range(100):
        xs = random.sample(range(100), random.randint(0, 100))
        __assert_sort(xs)

def __assert_sort(xs):
    zs = [x for x in xs]
    ys = sorted(xs)
    xs = sort(xs)
    if ys != xs:
        print "before sort: ", zs
        print "expected: ", ys
        print "got     : ", xs
        exit()

if __name__ == "__main__":
    test()

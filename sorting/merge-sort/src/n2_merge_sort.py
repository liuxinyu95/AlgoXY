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
    ordered = False
    while not ordered:
        (ordered, xs) = proc(xs)
    return xs

# take one pass
def proc(xs):
    n = len(xs)
    ys = [0]*n  #[xs[0]]*n
    (a, b) = (0, 0)
    (c, d) = (n, n)
    (f, r) = (0, n-1)
    t = True # put result from left if true, else put result from right
    while b < c:
        # span [a, b) as much as possible
        while True:
            b = b + 1
            if b == c - 1 or xs[b] < xs[b-1]:
                break
        # span [c, d) as much as possible
        while b < c:
            c = c - 1
            if c < n and xs[c-1] < xs[c]:
                break
        print "span [a, b)==>", a, b, "span [c, d) ==>", c, d
        if t:
            f = merge(xs[a:b], xs[c:d], ys, f, t)
        else:
            r = merge(xs[a:b], xs[c:d], ys, r, t)
        (a, d) = (b, c)
        t = not t
    print "pass result:", ys
    return (b - a - 1 == n, ys)

# TODO: merge ys from right to left!!!
def merge(xs, ys, zs, k, t):
    print "merge", xs, ys, zs, k, t

    delta = 1
    if not t:
        delta = -1

    while xs != [] and ys != []:
        if xs[0] < ys[0]:
            zs[k] = xs[0]
            xs = xs[1:]
        else:
            zs[k] = ys[0]
            ys = ys[1:]
        k = k + delta
    if xs != []:
        for x in xs:
            zs[k] = x
            k = k + delta
    if ys != []:
        for y in ys:
            zs[k] = y
            k = k + delta
    print "merge result:", zs
    return k

def test():
    for i in range(100):
        print i
        xs = random.sample(range(100), random.randint(0, 100))
        print xs
        assert(sorted(xs) == sort(xs))


if __name__ == "__main__":
    #print sort([2, 1, 3])
    print sort([78, 70, 99, 33, 88, 17, 62, 51, 18, 19, 47, 1, 63, 46, 53, 21, 69, 48, 55, 67, 13, 25, 98, 29])
    #test()

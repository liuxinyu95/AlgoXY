#!/usr/bin/python

# cdetect.py
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


# Cycle detection problem.

# Given a finite set S, and a function f on S. For any given value x0 in S, the
# sequence:
#   x0, x1 = f(x0), x2 = f(x1), ...,
# repeat from the k-th value, with period of n.

import random # for verification purpose only

class Node:
    def __init__(self, key, next = None):
        self.key = key
        self.next = next

def fnext(x):
    return x.next if x is not None else x

def prt(str, x):
    print str, None if x is None else x.key

# Robert W. Floyd method.
def find_cycle(x0, f):
    # p = q = x0, repeat p = f(p), q = f((q)) until p == q
    p = f(x0)     # the slow (tortoise)
    q = f(f(x0))  # the fast (hare)
    while p != q: # loop until converge
        p = f(p)
        q = f(f(q))

    k = 0
    q = x0        # reset the fast
    while p != q: # loop to the connection point
        p = f(p)
        q = f(q)
        k = k + 1

    n = 1
    q = f(p)
    while p != q: # traverse the circle
        q = f(q)
        n = n + 1

    return k, n

# Richard P. Brent method
def detect_cycle(x0, f):
    n = power = 1
    p = x0
    q = f(x0)
    while p != q: # loop until converge
        if power == n:
            p = q # reset the start point
            n = 0
            power = power * 2
        q = f(q)
        n = n + 1

    p = q = x0          # reset
    for _ in xrange(n): # make distance |qp| = n
        q = f(q)

    k = 0
    while p != q: # loop to the connection point
        p = f(p)
        q = f(q)
        k = k + 1

    return k, n

# verification

# create a linked-list of length n, if circular is true,
# make it loop starting from the k-th node.
def create(n, k, circular):
    t = p = None
    xs = range(n)
    random.shuffle(xs)
    for x in xs:
        p = Node(x, p)
        if t is None:
            t = p
    if circular:
        q = p
        for _ in xrange(k):
            q = q.next
        t.next = q
    return p

# get the k-th node of the linked-list p
def get_at(p, k):
    for _ in xrange(k):
        p = fnext(p)
    return p

def test():
    def __assert(str, p1, p2, circular):
        if p1 != p2:
            print "assert fail:", str, p1, p2, circular
            quit()
    for _ in xrange(1000):
        n = random.randint(1, 100)
        k = random.randint(0, n-1)
        circular = (random.randint(0, 1) == 0)
        p = create(n, k, circular)
        __assert("Floyd", find_cycle(p, fnext), (k, n-k) if circular else (n, 1), circular)
        __assert("Brent", detect_cycle(p, fnext),  (k, n-k) if circular else (n, 1), circular)
    print "1000 cases passed"

if __name__ == "__main__":
    test()

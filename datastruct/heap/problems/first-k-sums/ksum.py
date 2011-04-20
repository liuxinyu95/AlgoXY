#!/usr/bin/python

# ksum.py, find the first k biggest sum from 2 orderred arrays.
# Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)
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
import heapq

# method 1:
#   use binary heap (heapq in python lib)

# method 2:
#   use young tableau

class lazy_yong_tableau:
    def __init__(self, xs, ys):
        self.young = {}
        self.c = lambda i, j: xs[i]+ys[j]
        self.size = (len(xs), len(ys))
        self.infinity = xs[-1]+ys[-1]+1000

    def get(self, i, j):
        if (i,j) not in self.young:
            self.young[(i, j)]=self.c(i, j)
        return self.young[(i,j)]

    def set(self, i, j, x):
        self.young[(i, j)]=x
    
    def swap(self, i1, j1, i2, j2):
        x = self.get(i1, j1)
        y = self.get(i2, j2)
        self.set(i1, j1, y)
        self.set(i2, j2, x)

def young_pop(young):
    x = young.get(0, 0)
    young.set(0, 0, young.infinity)
    youngify(young, 0, 0)
    return x

def youngify(young, i, j):
    (m, n) = young.size
    while True:
        (min_i, min_j)=(i, j)
        if i+1 < m and young.get(i, j) > young.get(i+1, j):
            (min_i, min_j) = (i+1, j)
        if j+1 < n and young.get(min_i, min_j) > young.get(i, j+1):
            (min_i, min_j) = (i, j+1)
        if (min_i, min_j) != (i, j):
            young.swap(i, j, min_i, min_j)
            (i, j) = (min_i, min_j)
        else:
            break

def ksum_ytab(k, xs, ys):
    young = lazy_yong_tableau(xs, ys)
    res=[]
    for i in range(k):
        res.append(young_pop(young))
    return res

def ksum_heapq(k, xs, ys):
    res = []
    heap = [(x+ys[0], 0) for x in xs]
    heapq.heapify(heap)
    for _ in xrange(len(ys)):
        v, i = heapq.heappop(heap)
        res.append(v)
        if i+1<len(ys):
            heapq.heappush(heap, (v-ys[i]+ys[i+1], i+1))
    return res[:k]

def orderred_insert_by(f, lst, p):
    for i in range(len(lst)):
        if p == lst[i]:
            return
        if f(lst[i])>f(p):
            lst.insert(i, p)
            return
    lst.append(p)

def ksum_foo(k, xs, ys):
    c=lambda (i, j): xs[i]+ys[j]
    res = []
    q = [(0, 0)]
    while len(res)<k and q!=[]:
        (i, j)=q.pop(0)
        res.append(c((i, j)))
        if i+1<len(xs):
            orderred_insert_by(c, q, (i+1, j))
        if j+1<len(ys):
            orderred_insert_by(c, q, (i, j+1))
    return res[:k] 

# testing for verification

def brute_force(k, xs, ys):
    res= [x+y for x in xs for y in ys]
    res.sort()
    return res[:k]

def test():
    for i in range(10):
        n= random.randint(1, 1000)
        k = random.randint(1, n)
        a = random.sample(range(10000), n)
        a.sort()
        b = random.sample(range(10000), n)
        b.sort()
        if ksum_heapq(k, a, b) != brute_force(k, a, b):
            print "fail!" 
            print "a=", a
            print "b=", b
            print "k=", k
            print "brute-force:", brute_force(k, a, b)
            print "ksum:", ksum_heapq(k, a, b)
            return
    print "OK"

if __name__ == "__main__":
    test()

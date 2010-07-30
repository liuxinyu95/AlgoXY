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

def orderred_insert_by(f, lst, p):
    for i in range(len(lst)):
        if p == lst[i]:
            return
        if f(lst[i])>f(p):
            lst.insert(i, p)
            return
    lst.append(p)
    
def ksum(k, xs, ys):
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
        if ksum(k, a, b) != brute_force(k, a, b):
            print "fail!" 
            print "a=", a
            print "b=", b
            print "k=", k
            print "brute-force:", brute_force(k, a, b)
            print "ksum:", ksum(k, a, b)
            return
    print "OK"

if __name__ == "__main__":
    test()

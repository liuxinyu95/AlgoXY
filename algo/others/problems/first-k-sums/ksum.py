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

def ksum(k, xs, ys):
    res = [xs[0]+ys[0]]
    i = 1
    while len(res)<k:
        a = map((lambda x: ys[i]+x), xs[:i+1])
        b = map((lambda y: xs[i]+y), ys[:i+1])
        while a!=[] and b!=[] and len(res)<k:
            if a[0]<b[0]:
                res.append(a[0])
                a.pop(0)
            else:
                res.append(b[0])
                b.pop(0)
        if a==[]:
            res+=b
        else:
            res+=a
        i=i+1
    return res[:k]

def brute_force(k, xs, ys):
    res=[]
    for x in xs:
        for y in ys:
            res.append(x+y)
    res.sort()
    return res[0:k]

def test():
    for i in range(1):
        n=20#n = random.randint(1, 1000)
        k=5#k = random.randint(1, n)
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
    print "OK"

if __name__ == "__main__":
    test()

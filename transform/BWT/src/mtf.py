#!/usr/bin/python

# mtf.py, Move-to-front transformation
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

M = 256

# method 1, naive solution
def mtf(xs):
    ys = range(M)
    rs = []
    for x in xs:
        i = ys.index(ord(x))
        ys.pop(i)
        ys.insert(0, ord(x))
        rs.append(i)
    return rs

def imtf(xs):
    ys = range(M)
    rs = []
    for x in xs:
        y = ys[x]
        rs.append(chr(y))
        ys.pop(x)
        ys.insert(0, y)
    return rs

# method 1.1, naive method with manually element moving.
def mtf1(xs):
    ys = range(M)
    rs = []
    for x in xs:
        i = 0
        prev = ord(x)
        while ys[i] != ord(x):
            (prev, ys[i]) = (ys[i], prev) # swap
            i = i + 1
        ys[i] = prev
        rs.append(i)
    return rs

def imtf1(xs):
    ys = range(M)
    rs = []
    for x in xs:
        rs.append(chr(ys[x]))
        prev = ys[x]
        for i in range(x):
            (prev, ys[i]) = (ys[i], prev)
        ys[x] = prev
    return rs

def test_mtf(encode, decode, xs, msg):
    print "\ntesting MTF with", msg, "method"
    rs = encode(xs)
    print "mtf(", xs, ")=", rs
    ys = "".join(decode(rs))
    print "imtf(", rs, ")=", ys
    assert(xs == ys)

def test():
    xss = ["banana", "mississippi", "cocoa"];
    codec = [(mtf, imtf, "naive"), (mtf1, imtf1, "naive1")];
    for xs in xss:
        for (encode, decode, msg) in codec:
            test_mtf(encode, decode, xs, msg)

if __name__ == "__main__":
    test()

# Reference
# [1]. http://en.wikipedia.org/wiki/Move-to-front_transform
# [2]. http://www.cs.cmu.edu/~sleator/papers/Adaptive-data-compression.htm

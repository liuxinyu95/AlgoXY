#!/usr/bin/python

# kmp.py, The Knuth-Morris-Pratt string matching algorithm
# Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)
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

import random #only for verification
import string #only for verification

# Match pattern p in text w
#   return all candidate position
def kmp_match(w, p):
    n = len(w)
    m = len(p)
    fallback = fprefix(p)
    k = 0 # how many elements have been matched so far.
    res = []
    for i in range(n):
        while k > 0 and p[k] != w[i]:
            k = fallback[k] #fall back
        if p[k] == w[i]:
            k = k + 1
        if k == m:
            res.append(i+1-m)
            k = fallback[k-1] # look for next
    return res

# Prepare the `prefix-function'
#   t(i) = max {k: k<i and p[1, ..., k] `is suffix of` p[1, ..., i-1]}
def fprefix(p):
    m = len(p)
    t = [0]*m  # fallback table
    k = 0
    for i in range(2, m):
        while k>0 and p[i-1] != p[k]:
            k = t[k-1] #fallback
        if p[i-1] == p[k]:
            k = k + 1
        t[i] = k
    #print "ptn=", ", ".join(p)
    #print "tab=", t
    return t

# naive search for verification
#   O(m*n) algorithm
def naive_match(w, p):
    n = len(w)
    m = len(p)
    res = []
    for i in range(n):
        if w[i:(i+m)] == p:
            res.append(i)
    return res

def test():
    for k in xrange(100):
        n = random.randint(10, 10000)
        w = [random.choice(string.ascii_lowercase) for i in xrange(n)]
        i = random.randint(0, n-2)
        j = random.randint(i+1, n)
        p = w[i:j]
        __assert(kmp_match(w, p), naive_match(w, p))
    print "100 test cases run OK"

def __assert(x, y):
    if x!=y:
        print "left:", x, "right:", y

if __name__ == "__main__":
    test()

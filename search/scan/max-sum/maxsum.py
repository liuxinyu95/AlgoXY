#!/usr/bin/python

# maxsum.py
# Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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

# refer to column 7 in [1]
# [1]. Jon Bentley. ``Programming pearls, Second Edition''. Addison-Wesley Professional; 1999. ISBN-13: 978-0201657883

import random

# invariant
# .... max so far .... max end here
def maxsum(xs):
    s = 0 # max so far
    m = 0 # max end here
    for x in xs:
        m = max(m + x, 0)
        s = max(m, s)
    return s

# divide and conquer solution
def dc_maxsum(xs):
    n = len(xs)
    if n == 0:
        return 0
    if n == 1:
        return max(0, xs[0])
    m = n //2
    a = max_from(reversed(xs[:m]))
    b = max_from(xs[m:])
    return max(dc_maxsum(xs[:m]), dc_maxsum(xs[m:]), a + b)

def max_from(xs):
    s = 0
    m = 0
    for x in xs:
        s = s + x
        m = max(m, s)
    return m

def naive_maxsum(xs):
    n = len(xs)
    m = 0
    r = (0, 0)
    for i in range(n):
        s = 0
        for j in range(i, n):
            s = s + xs[j]
            if m < s:
                m = s
                r = (i, j)
    return (m, r)

def test_maxsum(f):
    for _ in range(100):
        xs = random.sample(range(-100, 100), 100)
        (m, _) = naive_maxsum(xs)
        assert m == f(xs)

def test():
    test_maxsum(maxsum)
    test_maxsum(dc_maxsum)

def example():
    xs = [3, -13, 19, -12, 1, 9, 18, -16, 15, -15]
    print naive_maxsum(xs)

if __name__ == "__main__":
    example()
    test()

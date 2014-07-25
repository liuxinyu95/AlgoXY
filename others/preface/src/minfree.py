#!/usr/bin/python

# minfree.py, find the smallest free numbers
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

def min_free(lst):
    n = len(lst)
    a = [0]*(n+1)
    for x in lst:
        if x < n:
            a[x] = 1
    return a.index(0)

def brute_force(lst):
    i = 0
    while True:
        if i not in lst:
            return i
        i = i + 1

def dc_min_free(lst):
    return binary_search(lst, 0, len(lst)-1)

def binary_search(lst, l, u):
    if lst == []:
        return l
    m = (l + u ) / 2
    xs = [x for x in lst if x <= m]
    ys = [x for x in lst if x > m]
    if len(xs) == m - l + 1:
        return binary_search(ys, m+1, u)
    else:
        return binary_search(xs, l, m)

def test():
    lst = [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6]
    assert(min_free(lst), brute_force(lst))
    assert(dc_min_free(lst), brute_force(lst))
    assert(min_free(range(5)), brute_force(range(5)))

if __name__ == "__main__":
    test()

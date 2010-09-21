#!/usr/bin/python

# bheap.py, binary heap (like heapq in python lib)
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

# auxiliary functions 

def parent(i):
    return i//2

def left(i):
    return  2*i

def right(i):
    return 2*i+1

# max-heapify by default
def heapify(x, i):
    heapify_by(lambda a, b: a<b, x, i) 

def heapify_by(less_p, x, i):
    n = len(x)
    while True:
        l = left(i)
        r = right(i)
        largest = i
        if l < n and less_p(x[i], x[l]):
            largest = l
        if r < n and less_p(x[largest], x[r]):
            largest = r
        if largest != i:
            (x[i], x[largest])=(x[largest], x[i])
            i  = largest
        else:
            break

# build max heap by default
def build_heap(x):
    n = len(x)
    for i in reversed(range(n//2)):
        heapify(x, i)

class TestHeap:
    def __init__(self):
        print "Implicit binary heap by array testing"

    def run(self):
        self.test_heapify()

    def test_heapify(self):
        l = [27, 17, 3, 16, 13, 10, 1, 5, 7, 12, 4, 8, 9, 0]
        heapify(l, 2)
        print l

if __name__ == "__main__":
    TestHeap().run()

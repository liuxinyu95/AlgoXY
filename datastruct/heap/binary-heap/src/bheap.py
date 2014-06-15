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


# pre-defined heap type
MIN_HEAP = lambda a, b: a < b
MAX_HEAP = lambda a, b: a > b

# auxiliary functions

def parent(i):
    return (i+1)//2-1

def left(i):
    return  2*i+1

def right(i):
    return 2*(i+1)

# min-heapify by default
def heapify(x, i, less_p = MIN_HEAP):
    n = len(x)
    while True:
        l = left(i)
        r = right(i)
        smallest = i
        if l < n and less_p(x[l], x[i]):
            smallest = l
        if r < n and less_p(x[r], x[smallest]):
            smallest = r
        if smallest != i:
            (x[i], x[smallest])=(x[smallest], x[i])
            i  = smallest
        else:
            break

# build min heap by default
def build_heap(x, less_p = MIN_HEAP):
    n = len(x)
    for i in reversed(range(n//2)):
        heapify(x, i, less_p)

def heap_top(x):
    return x[0] #ignore empty case

# default apply to min-heap
def heap_pop(x, less_p = MIN_HEAP):
    top = heap_top(x)
    x[0] = x[-1] # this is faster than top = x.pop(0)
    x.pop()
    if x!=[]:
        heapify(x, 0, less_p)
    return top

# default heap sort less to greater
def heap_sort(x, less_p = MIN_HEAP):
    res = []
    build_heap(x, less_p)
    while x!=[]:
        res.append(heap_pop(x, less_p))
    return res

# decrease key in min-heap by default
def heap_decrease_key(x, i, key, less_p = MIN_HEAP):
    if less_p(key, x[i]):
        x[i] = key
        heap_fix(x, i, less_p)

# insert a key to min-heap by default
def heap_insert(x, key, less_p = MIN_HEAP):
    i = len(x)
    x.append(key)
    heap_fix(x, i, less_p)

def heap_fix(x, i, less_p = MIN_HEAP):
    while i>0 and less_p(x[i],x[parent(i)]):
        (x[parent(i)], x[i]) = (x[i], x[parent(i)])
        i = parent(i)

def top_k(x, k, less_p = MIN_HEAP):
    build_heap(x, less_p)
    return [heap_pop(x, less_p) for _ in range(min(k, len(x)))]

class TestHeap:
    def __init__(self):
        print "Implicit binary heap by array testing"

    def run(self):
        self.test_heapify()
        self.test_build_heap()
        self.test_heap_sort()
        self.test_heap_decrease_key()
        self.test_heap_insert()
        self.test_top_k()

    def __assert(self, p):
        if p:
            print "OK"
        else:
            print "Fail!"

    def test_heapify(self):
        # CLRS Figure 6.2
        l = [16, 4, 10, 14, 7, 9, 3, 2, 8, 1]
        heapify(l, 1, MAX_HEAP)
        print l
        self.__assert(l == [16, 14, 10, 8, 7, 9, 3, 2, 4, 1])

    def test_build_heap(self):
        # CLRS Figure 6.3
        l = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7]
        build_heap(l, MAX_HEAP)
        print l
        self.__assert(l == [16, 14, 10, 8, 7, 9, 3, 2, 4, 1])

    def test_heap_sort(self):
        # CLRS Figure 6.4
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        res = heap_sort(l)
        print res
        self.__assert(res == [1, 2, 3, 4, 7, 8, 9, 10, 14, 16])

    def test_heap_decrease_key(self):
        # CLRS Figure 6.5
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        heap_decrease_key(l, 8, 15, MAX_HEAP)
        print l
        self.__assert(l == [16, 15, 10, 14, 7, 9, 3, 2, 8, 1])

    def test_heap_insert(self):
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        heap_insert(l, 17, MAX_HEAP)
        print l
        self.__assert(l == [17, 16, 10, 8, 14, 9, 3, 2, 4, 1, 7])

    def test_top_k(self):
        l = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7]
        res = top_k(l, 3, MAX_HEAP)
        print res
        self.__assert(res == [16, 14, 10])

if __name__ == "__main__":
    TestHeap().run()

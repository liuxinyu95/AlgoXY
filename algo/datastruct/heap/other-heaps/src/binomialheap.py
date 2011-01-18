#!/usr/bin/python

# binomialheap.py, binomial heap 
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


# Assume the heap is min-heap

# Use left child, right sibling approach
class BinomialTree:
    def __init__(self):
        rank = 0
        key = None
        parent = None
        child = None
        sibling = None

# Auxiliary function to extract the first tree
def extract_first(h):
    t = None
    if h is not None:
        t = h
        h = h.sibling
        t.sibling = None
    return (t, h)

# Assume h is not empty
def find_min(h):
    x = h.key
    while h.sibling is not None:
        h = h.sibling
        if h.key < x:
            x = h.key
    return x

# Implicit condition that the rank of the two trees are same       
def link(t1, t2):
    if t2.key < t1.key:
        (t1, t2) = (t2, t1)
    t1.sibling = t2
    t2.parent = t1
    t1.rank = t1.rank + 1
    #release t2
    return t1

def insert_tree(h, t):
    head = h
    p = None
    while h is not None and t.rank > h.rank:
        p = h
        h = h.sibling
    t.sibling = h
    if p is not None:
        p.sibling = t
    else:
        head = t
    return head

def merge(h1, h2):
    if h1 is None:
        return h2
    if h2 is None:
        return h1
    h = None
    while h1 is not None and h2 is not None:
        t = None
        if h1.rank < h2.rank :
            (t, h1) = extract_first(h1)
        elif h2.rank < h1.rank :
            (t, h2) = extract_first(h2)
        else:
            (t1, h1) = extract_first(h1)
            (t2, h2) = extract_first(h2)
            t = link(t1, t2)
        h = insert_tree(h, t)
    if h1 is not None:
        

# default heap sort less to greater
def heap_sort(x, less_p = MIN_HEAP):
    res = []
    build_heap(x, less_p)
    while x!=[]:
        res.append(heap_pop(x, less_p))
    return res

def top_k(x, k, less_p = MIN_HEAP):
    build_heap(x, less_p)
    return [heap_pop(x, less_p) for i in range(min(k, len(x)))]

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

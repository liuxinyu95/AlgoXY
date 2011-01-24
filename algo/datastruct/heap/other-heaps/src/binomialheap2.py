#!/usr/bin/python

# binomialheap2.py, binomial heap, implemented with list 
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

import random # for testing only

# Assume the heap is min-heap

# binomialheap.py uses ``left child, right sibling'' way as mentioned
# in CLRS[1]. I think by replacing with list, it can be simplified
class BinomialTree:
    def __init__(self, x = None):
        self.rank = 0
        self.key = x
        self.parent = None
        self.children = []

# Heap is list of [BinomialTree]

# Implicit condition that the rank of the two trees are same       
def link(t1, t2):
    if t2.key < t1.key:
        (t1, t2) = (t2, t1)
    t1.children.insert(0, t2)
    t1.rank = t1.rank + 1
    # release t2
    return t1

# Insert a tree to the proper position in the heap
# So that the trees are in monotonically increase order by rank
# Implicit condition: the rank of tree is lower or equal to the
# first tree in the heap
def insert_tree(ts, t):
    while ts !=[] and t.rank == ts[0].rank:
        t = link(t, ts.pop(0))
    ts.insert(0, t)
    return ts

# Insertion
def insert(h, x):
    return insert_tree(h, BinomialTree(x))

# Append a tree to the heap, so that the trees are in
# monotonically increase order by rank
# Implicit condition: the rank of tree is equal to or bigger by 1 than
# the last tree in the heap.
def append_tree(ts, t):
    if ts != [] and ts[-1].rank == t.rank:
        ts[-1] = link(ts[-1], t)
    else:
        ts.append(t)
    return ts

# Helper function to append a heap to another one by repeatedly calling
# append_tree()
def append_trees(ts1, ts2):
    return reduce(append_tree, ts2, ts1)

# Merge 2 heaps together. Use a merge sort like approach
def merge(ts1, ts2):
    if ts1 == []:
        return ts2
    if ts2 == []:
        return ts1
    ts = []
    while ts1 != [] and ts2 != []:
        t = None
        if ts1[0].rank < ts2[0].rank:
            t = ts1.pop(0)
        elif ts2[0].rank < ts1[0].rank:
            t = ts2.pop(0)
        else:
            t = link(ts1.pop(0), ts2.pop(0))
        ts = append_tree(ts, t)
    ts = append_trees(ts, ts1)
    ts = append_trees(ts, ts2)
    return ts

# Extract the minimum binomial tree from the heap
# returns (min tree, rest trees)
def remove_min_tree(ts):
    min_t = min(ts, key=lambda t: t.key)
    ts.remove(min_t)
    return (min_t, ts)    

# Assume ts is not empty
def find_min(ts):
    min_t = min(ts, key=lambda t: t.key)
    return min_t.key

# Extract the min element, returns the (min, heap')
def extract_min(ts):
    (min_t, ts) = remove_min_tree(ts)
    min_t.children.reverse()
    ts = merge(ts, min_t.children)
    min_t.children = []
    return (min_t.key, ts)

# helper function
def from_list(lst):
    return reduce(insert, lst, [])

def heap_sort(lst):
    h = from_list(lst)
    res = []
    while h != []:
        (x, h) = extract_min(h)
        res.append(x)
    return res

def to_string(ts):
    s = ""
    for t in ts:
        s = s+ "(" + str(t.key)
        if t.children != []:
            s = s + ", " + to_string(t.children)
        s = s+ ")"
    return s

class TestHeap:
    def __init__(self):
        print "Binomial heap testing"

    def run(self):
        self.test_insert()
        self.test_heap_sort()
        self.test_random_sort()
        #self.test_heap_decrease_key()

    def __assert(self, p):
        if p:
            print "OK"
        else:
            print "Fail!"

    def test_insert(self):
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        print to_string(from_list(l))

    def test_heap_sort(self):
        # CLRS Figure 6.4
        l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        res = heap_sort(l)
        print res
        self.__assert(res == [1, 2, 3, 4, 7, 8, 9, 10, 14, 16])

    def test_random_sort(self):
        n = 1000
        for i in range(100):
            lst = random.sample(range(n), random.randint(0, n))
            assert(heap_sort(lst) == sorted(lst))
        print "OK"
        

    #def test_heap_decrease_key(self):
        # CLRS Figure 6.5
        #l = [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
        #heap_decrease_key(l, 8, 15, MAX_HEAP)
        #print l
        #self.__assert(l == [16, 15, 10, 14, 7, 9, 3, 2, 8, 1])

if __name__ == "__main__":
    TestHeap().run()

# Reference
# [1]. CLRS. Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest and Clifford Stein. ``Introduction to Algorithms, Second Edition''. The MIT Press, 2001. ISBN: 0262032937.

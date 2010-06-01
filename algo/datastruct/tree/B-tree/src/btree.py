#!/usr/bin/python

# btree.py, B-Tree
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

TREE_2_3_4 = 2 #by default, create 2-3-4 tree

class BTreeNode:
    def __init__(self, t=TREE_2_3_4, leaf=True):
        self.leaf = leaf
        self.t = t
        self.keys = [] #self.data = ...
        self.children = []

    # self: (...x, key[i]=k, ...) ==> 
    # self: (...x, key'[i], y, key'[i+1]=k...)
    def split_child(self, i):
        t = self.t
        x = self.children[i]
        y = BTreeNode(t, x.leaf)
        self.keys.insert(i, x.keys[t-1])
        self.children.insert(i+1, y)
        y.keys = x.keys[t:]
        x.keys = x.keys[:t-1]
        if not y.leaf:
            y.children = x.children[t:]
            x.children = x.children[:t]
        #disk_write(y)
        #disk_write(z)
        #disk_write(x)

    def is_full(self):
        return len(self.keys) == 2*self.t-1

# t: minimum degree
def B_tree_create(t=TREE_2_3_4):
    x = BTreeNode(t)
    #disk_write(x) 
    return x

def B_tree_insert(tr, key): # + data parameter
    root = tr
    if root.is_full():
        s = BTreeNode(root.t, False)
        s.children.insert(0, root)
        s.split_child(0)
        root = s
    B_tree_insert_nonfull(root, key)
    return root

def orderred_insert(lst, x):
    i = len(lst)
    lst.append(x)
    while i>0 and lst[i]<lst[i-1]:
        (lst[i-1], lst[i]) = (lst[i], lst[i-1])
        i=i-1

def B_tree_insert_nonfull(tr, key):
    if tr.leaf:
        orderred_insert(tr.keys, key)
        #disk_write(tr)
    else:
        i = len(tr.keys)
        while i>0 and key < tr.keys[i-1]:
            i=i-1
        #disk_read(tr.children[i])
        if tr.children[i].is_full():
            tr.split_child(i)
            if key> tr.keys[i-1]:
                i = i+1
        B_tree_insert_nonfull(tr.children[i], key)
    
def B_tree_to_str(tr):
    res = "("
    if tr.leaf:
        res += ", ".join(tr.keys)
    else:
        for i in range(len(tr.keys)):
            res+= B_tree_to_str(tr.children[i]) + ", " + tr.keys[i] + ", "
        res += B_tree_to_str(tr.children[len(tr.keys)])
    res += ")"
    return res

#t: minimum degree
def list_to_B_tree(l, t=TREE_2_3_4):
    tr = B_tree_create(t)
    for x in l:
        tr = B_tree_insert(tr, x)
    return tr

class BTreeTest:
    def __init__(self):
        print "B-tree testing"

    def run(self):
        self.test_insert()

    def test_insert(self):
        lst = ["G", "M", "P", "X", "A", "C", "D", "E", "J", "K", \
               "N", "O", "R", "S", "T", "U", "V", "Y", "Z"]
        print "2-3-4 tree of", lst
        tr = list_to_B_tree(lst)
        print B_tree_to_str(tr)
        print "B-tree with t=3 of", lst
        print B_tree_to_str(list_to_B_tree(lst, 3))

if __name__ == "__main__":
    BTreeTest().run()

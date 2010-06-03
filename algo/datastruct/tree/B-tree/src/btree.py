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

    def merge_children(self, i):
        #merge children[i] and children[i+1] by pushing keys[i] down
        self.children[i].keys += [self.keys[i]]+self.children[i+1].keys
        self.children[i].children += self.children[i+1].children
        self.keys.pop(i)
        self.children.pop(i+1)
        #disk_write(self)
        #disk_write(self.children[i])

    def replace_key(self, i, key):
        self.keys[i] = key
        #disk_write(self)
        return key

    def is_full(self):
        return len(self.keys) == 2*self.t-1

    def can_remove(self):
        return len(self.keys) >= self.t

# t: minimum degree
def B_tree_create(t=TREE_2_3_4):
    x = BTreeNode(t)
    #disk_write(x) 
    return x

# insertion
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
            i = i-1
        #disk_read(tr.children[i])
        if tr.children[i].is_full():
            tr.split_child(i)
            if key>tr.keys[i]:
                i = i+1
        B_tree_insert_nonfull(tr.children[i], key)

# deletion
def B_tree_delete(tr, key):
    i = len(tr.keys)
    while i>0:
        if key == tr.keys[i-1]:
            if tr.leaf:  # case 1 in CLRS
                tr.keys.remove(key)
                #disk_write(tr)
            else: # case 2 in CLRS
                if tr.children[i-1].can_remove(): # case 2a
                    key = tr.replace_key(i-1, tr.children[i-1].keys[-1])
                    B_tree_delete(tr.children[i-1], key)
                elif tr.children[i].can_remove(): # case 2b
                    key = tr.replace_key(i-1, tr.children[i].keys[0])
                    B_tree_delete(tr.children[i], key1)
                else: # case 2c
                    tr.merge_children(i-1)
                    B_tree_delete(tr.children[i-1], key)
                    if tr.keys==[]: # tree shrinks in height
                        tr = tr.children[i-1]
            return tr
        elif key > tr.keys[i-1]:
            break
        else:
            i = i-1
    # case 3
    if tr.leaf:
        return tr #key doesn't exist at all
    if not tr.children[i].can_remove():
        if i>0 and tr.children[i-1].can_remove(): #left sibling
            tr.children[i].keys.insert(0, tr.keys[i])
            tr.keys[i] = tr.children[i-1].keys.pop()
            if not tr.children[i].leaf:
                tr.children[i].children.insert(tr.children[i-1].children.pop())
        elif i<len(tr.children) and tr.children[i+1].can_remove(): #right sibling
            tr.children[i].keys.append(tr.keys[i])
            tr.keys[i]=tr.children[i+1].keys.pop(0)
            if not tr.children[i].leaf:
                tr.children[i].children.append(tr.children[i+1].children.pop(0))
        else: # case 3b
            if i>0:
                tr.merge_children(i-1)
            else:
                tr.merge_children(i)
    B_tree_delete(tr.children[i], key)
    if tr.keys==[]: # tree shrinks in height
        tr = tr.children[0]
    return tr
    
def B_tree_search(tr, key):
    pass

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
        self.test_delete()

    def test_insert(self):
        lst = ["G", "M", "P", "X", "A", "C", "D", "E", "J", "K", \
               "N", "O", "R", "S", "T", "U", "V", "Y", "Z"]
        print "2-3-4 tree of", lst
        tr = list_to_B_tree(lst)
        print B_tree_to_str(tr)
        print "B-tree with t=3 of", lst
        print B_tree_to_str(list_to_B_tree(lst, 3))

    def test_delete(self):
        print "test delete"
        t = 3
        tr = BTreeNode(t, False)
        tr.keys=["P"]
        tr.children=[BTreeNode(t, False), BTreeNode(t, False)]
        tr.children[0].keys=["C", "G", "M"]
        tr.children[0].children=[BTreeNode(t), BTreeNode(t), BTreeNode(t), BTreeNode(t)]
        tr.children[0].children[0].keys=["A", "B"]
        tr.children[0].children[1].keys=["D", "E", "F"]
        tr.children[0].children[2].keys=["J", "K", "L"]        
        tr.children[0].children[3].keys=["N", "O"]
        tr.children[1].keys=["T", "X"]
        tr.children[1].children=[BTreeNode(t), BTreeNode(t), BTreeNode(t)]
        tr.children[1].children[0].keys=["Q", "R", "S"]
        tr.children[1].children[1].keys=["U", "V"]
        tr.children[1].children[2].keys=["Y", "Z"]
        print B_tree_to_str(tr)
        lst = ["F", "M", "G", "D", "B"]
        reduce(self.__test_del__, lst, tr)

    def __test_del__(self, tr, key):
        print "delete", key
        tr = B_tree_delete(tr, key)
        print B_tree_to_str(tr)
        return tr

if __name__ == "__main__":
    BTreeTest().run()

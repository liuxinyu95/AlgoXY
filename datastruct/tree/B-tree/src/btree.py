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

TREE_2_3_4 = 2

class BTree:
    def __init__(self):
        self.keys = []
        self.subtrees = []

# d - 1 <= |t.keys| <= 2 * d - 1

def is_leaf(t):
    return t.subtrees == []

def full(d, t):
    return len(t.keys) >= 2 * d - 1

def low(d, t):
    return len(t.keys) <= d

def merge_subtrees(tr, i):
    """merge subtrees[i], keys[i], subtrees[i+1]"""
    tr.subtrees[i].keys += [tr.keys[i]] + tr.subtrees[i + 1].keys
    tr.subtrees[i].subtrees += tr.subtrees[i + 1].subtrees
    tr.keys.pop(i)
    tr.subtrees.pop(i + 1)

def split(d, z, i):
    """split z.subtrees[i] at degree d"""
    x = z.subtrees[i]
    y = BTree()
    z.keys.insert(i, x.keys[d - 1])
    z.subtrees.insert(i + 1, y)
    y.keys = x.keys[d : ]
    x.keys = x.keys[ : d - 1]
    if not is_leaf(x):
        y.subtrees = x.subtrees[d : ]
        x.subtrees = x.subtrees[ : d]

def insert(d, t, x):
    """insert key x to tree t, where the degree is d"""
    root = t
    if full(d, root):
        s = BTree()
        s.subtrees = [root]
        split(d, s, 0)
        root = s
    return insert_nonfull(d, root, x)

def insert_nonfull(d, t, x):
    if is_leaf(t):
        ordered_insert(t.keys, x)
    else:
        i = len(t.keys)
        while i > 0 and key < t.keys[i-1]:
            i = i - 1
        if full(d, t.subtrees[i]):
            split(d, t, i)
            if key > t.keys[i]:
                i = i + 1
        insert_nonfull(d, t.subtrees[i], x)
    return t

def ordered_insert(lst, x):
    i = len(lst)
    lst.append(x)
    while i > 0 and lst[i] < lst[i - 1]:
        (lst[i - 1], lst[i]) = (lst[i], lst[i - 1])
        i = i - 1

def B_tree_delete(tr, key):
    def replace_key(tr, i, k):
        tr.keys[i] = k
        return k
    i = len(tr.keys)
    while i>0:
        if key == tr.keys[i-1]:
            if is_leaf(tr):  # case 1 in CLRS
                tr.keys.remove(key)
                #disk_write(tr)
            else: # case 2 in CLRS
                if can_remove(tr.subtrees[i-1]): # case 2a
                    key = replace_key(tr, i-1, tr.subtrees[i-1].keys[-1])
                    B_tree_delete(tr.subtrees[i-1], key)
                elif can_remove(tr.subtrees[i]): # case 2b
                    key = replace_key(tr, i-1, tr.subtrees[i].keys[0])
                    B_tree_delete(tr.subtrees[i], key)
                else: # case 2c
                    merge_subtrees(tr, i-1)
                    B_tree_delete(tr.subtrees[i-1], key)
                    if tr.keys==[]: # tree shrinks in height
                        tr = tr.subtrees[i-1]
            return tr
        elif key > tr.keys[i-1]:
            break
        else:
            i = i-1
    # case 3
    if is_leaf(tr):
        return tr #key doesn't exist at all
    if not can_remove(tr.subtrees[i]):
        if i>0 and can_remove(tr.subtrees[i-1]): #left sibling
            tr.subtrees[i].keys.insert(0, tr.keys[i-1])
            tr.keys[i-1] = tr.subtrees[i-1].keys.pop()
            if not is_leaf(tr.subtrees[i]):
                tr.subtrees[i].subtrees.insert(0, tr.subtrees[i-1].subtrees.pop())
        elif i<len(tr.subtrees) and can_remove(tr.subtrees[i+1]): #right sibling
            tr.subtrees[i].keys.append(tr.keys[i])
            tr.keys[i]=tr.subtrees[i+1].keys.pop(0)
            if not is_leaf(tr.subtrees[i]):
                tr.subtrees[i].subtrees.append(tr.subtrees[i+1].subtrees.pop(0))
        else: # case 3b
            if i>0:
                merge_subtrees(tr, i-1)
            else:
                merge_subtrees(tr, i)
    B_tree_delete(tr.subtrees[i], key)
    if tr.keys==[]: # tree shrinks in height
        tr = tr.subtrees[0]
    return tr

def lookup(t, k):
    """lookup key k in tree t"""
    for i in range(len(t.keys)):
        if k <= t.keys[i]:
            break
    if key == t.keys[i]:
        return (t, i)
    elif is_leaf(t):
        return None
    else:
        if k > t.keys[-1]:
            i = i + 1
        return lookup(t.subtrees[i], k)

def fromlist(d, xs):
    t = BTree()
    for x in xs:
        t = insert(d, t, x)
    return t

def tolist(t):
    xs = []
    if t is None:
        return xs
    if is_leaf(t):
        xs = [k for k in t.keys]
    else:
        for i in range(len(t.keys)):
            xs += tolist(t.subtrees[i])
            xs.append(t.keys[i])
        xs += tolist(t.subtrees[-1])
    return xs

def B_tree_to_str(tr):
    res = "("
    if is_leaf(tr):
        res += ", ".join(tr.keys)
    else:
        for i in range(len(tr.keys)):
            res+= B_tree_to_str(tr.subtrees[i]) + ", " + tr.keys[i] + ", "
        res += B_tree_to_str(tr.subtrees[len(tr.keys)])
    res += ")"
    return res

if __name__ == "__main__":
    # run tests

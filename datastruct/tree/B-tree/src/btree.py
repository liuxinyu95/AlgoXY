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

from random import sample, randint

TREE_2_3_4 = 2

class BTree:
    def __init__(self):
        self.keys = []
        self.subtrees = []

    def __str__(self):
        res = "("
        if is_leaf(self):
            res += ", ".join(str(k) for k in self.keys)
        else:
            for i in range(len(self.keys)):
                res += str(self.subtrees[i]) + ", " + str(self.keys[i]) + ", "
            res += str(self.subtrees[-1])
        return res + ")"

# d - 1 <= |t.keys| <= 2 * d - 1
#
# To avoid violating B-tree after insert/delete, when keys reach to either boundary
# (= hold), trigger split/merge

def is_leaf(t):
    return t.subtrees == []

def full(d, t):
    return len(t.keys) >= 2 * d - 1

def low(d, t):
    return len(t.keys) <= d - 1

def merge_subtrees(t, i):
    """merge t.subtrees[i], keys[i], t.subtrees[i+1]"""
    t.subtrees[i].keys += [t.keys[i]] + t.subtrees[i + 1].keys
    t.subtrees[i].subtrees += t.subtrees[i + 1].subtrees
    t.keys.pop(i)
    t.subtrees.pop(i + 1)

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
        while i > 0 and x < t.keys[i-1]:
            i = i - 1
        if full(d, t.subtrees[i]):
            split(d, t, i)
            if x > t.keys[i]:
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
    if t.keys == []:
        return None
    for i in range(len(t.keys)):
        if k <= t.keys[i]:
            break
    if k == t.keys[i]:
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

# verification

def deg(xs):
    return (xs[0] % 5) + 2 if xs else 2

def is_btree(d, t, depth):
    if t is None:
        return True
    n = len(t.keys)
    if n > 2 * d - 1:
        return False
    if depth > 0 and n < d - 1:
        return False
    for tr in t.subtrees:
        if not is_btree(d, tr, depth + 1):
            return False
    return True

def prop_order(xs):
    t = fromlist(deg(xs), xs)
    ys = tolist(t)
    zs = sorted(xs)
    assert ys == zs, f"ys = {ys}, zs = {zs}, t = {t}"

def prop_insert(xs):
    d = deg(xs)
    t = fromlist(d, xs)
    assert is_btree(d, t, 0), f"violate B-tree: d = {d}, t = {t}"

def prop_lookup(xs):
    d = deg(xs)
    t = fromlist(d, xs)
    ys = sample(xs, min(5, len(xs))) + sample(range(100), 5)
    for y in ys:
        r = lookup(t, y)
        if y in xs:
            assert r, f"not found {y} in t = {t}"
            (tr, i) = r
            assert tr.keys[i] == y, f"y = {y}, tr = {tr}, i = {i}"
        else:
            assert (r is None), f"y = {y}, r = {r}"

def test(f):
    for _ in range(100):
        xs = sample(range(100), randint(0, 100))
        f(xs)
    print(f"100 tests for {f} passed.\n")

if __name__ == "__main__":
    test(prop_order)
    test(prop_insert)
    test(prop_lookup)

#!/usr/bin/python

# prefixtree.py, Alphabetic prefix tree
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

from itertools import count

# Prefix tree definition
class PrefixTree:
    def __init__(self, value = None):
        self.value = value
        self.subtrees = {}

# longest common prefix
# returns (p, s1', s2'), where p is lcp, s1'=s1-p, s2'=s2-p
def lcp(s1, s2):
    j = 0
    while j < len(s1) and j < len(s2) and s1[j] == s2[j]:
        j += 1
    return (s1[0:j], s1[j:], s2[j:])

def branch(key1, tree1, key2, tree2):
    if key1 == []:
        tree1.subtrees[key2] = tree2
        return tree1
    t = PrefixTree()
    t.subtrees[key1] = tree1
    t.subtrees[key2] = tree2
    return t

def insert(t, key, value):
    if t is None:
        t = PrefixTree()
    node = t
    while True:
        match = False
        for k, tr in node.subtrees.items():
            if key == k: # overwrite
                tr.value = value
                return t
            prefix, k1, k2 = lcp(key, k)
            if prefix != "":
                match = True
                if k2 == "":  # go on traversing
                    node = tr
                    key = k1
                    break
                else: #branch out a new leaf
                    node.subtrees[prefix] = branch(k1, PrefixTree(value), k2, tr)
                    del node.subtrees[k]
                    return t
        if not match: # add a new leaf
            node.subtrees[key] = PrefixTree(value)
            break
    return t

def lookup(t, key):
    if t is None:
        return None
    while True:
        match = False
        for k, tr in t.subtrees.items():
            if k == key:
                return tr.value
            prefix, k1, k2 = lcp(key, k)
            if prefix != "" and k2 == "":
                match = True
                key = k1
                t = tr
                break
        if not match:
            break
    return None

def keys(t, prefix = ""):
    ks = []
    if t.value is not None:
        ks.append(prefix)
    for k, tr in sorted(t.subtrees.items()):
        ks += keys(tr, prefix + k)
    return ks

def from_list(kvs):
    t = None
    for k, v in kvs:
        t = insert(t, k, v)
    return t

def from_map(m):
    return from_list(m.items())

def from_text(txt):
    return from_list(zip(txt.split(), count()))

def test():
    n = 100
    txts = ["a another an boy bool zoo",
            "zoo bool boy another an a",
            "zoo is a place where animals are for public to see",
            "an zoo example with duplicated zoo words"]
    for txt in txts:
        kvs = list(zip(txt.split(), count()))
        m = dict(kvs)
        print("kvs", kvs, "\nmap", m)
        t = from_list(kvs)
        for k in m:
            v = lookup(t, k)
            assert(v is not None)
            assert(v == m[k])
        assert(sorted(keys(t)) == sorted(m.keys()))
    print(len(txts), "tests passed.")

if __name__ == "__main__":
    test()

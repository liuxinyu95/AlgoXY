#!/usr/bin/python

# trie.py, Alphabetic Trie.
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

import string
from trieutil import *

class Trie:
    def __init__(self):
        self.value = None
        self.children = {}

def trie_insert(t, key, value = None):
    if t is None:
        t = Trie()

    p = t
    for c in key:
        if not c in p.children: 
            p.children[c] = Trie()
        p = p.children[c]
    p.value = value
    return t

def lookup(t, key):
    if t is None:
        return None

    p = t
    for c in key:
        if not c in p.children:
            return None
        p = p.children[c]
    return p.value

def list_to_trie(l):
    return from_list(l, trie_insert)

def map_to_trie(m):
    return from_map(m, trie_insert)

class TrieTest:
    def __init__(self):
        print "start trie test"

    def run(self):
        self.test_insert()
        self.test_lookup()

    def test_insert(self):
        t = None
        t = trie_insert(t, "a")
        t = trie_insert(t, "an")
        t = trie_insert(t, "another")
        t = trie_insert(t, "b")
        t = trie_insert(t, "bob")
        t = trie_insert(t, "bool")
        t = trie_insert(t, "home")
        print trie_to_str(t)
        t1 = list_to_trie(["011", "11", "0011"])
        print trie_to_str(t1)
        t2 = map_to_trie({"001":'y', "100":'x', "101":'z'})
        print trie_to_str(t2)

    def test_lookup(self):
        t = map_to_trie({"a":1, "an":2, "another":7, "b":1, "bool":4, "bob":3, "home":4})
        print "find another: ", lookup(t, "another")
        print "find home: ", lookup(t, "home")
        print "find the: ", lookup(t, "the")

if __name__ == "__main__":
    TrieTest().run()

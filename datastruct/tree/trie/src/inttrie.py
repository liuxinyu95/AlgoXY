#!/usr/bin/python

# inttrie.py, Integer base Trie.
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
from trieutil import from_list, from_map

class IntTrie:
    def __init__(self):
        self.value = None
        self.left = self.right = None

def trie_insert(t, key, value = None):
    if t is None:
        t = IntTrie()

    p = t
    while key != 0:
        if key & 1 == 0:
            if p.left is None:
                p.left = IntTrie()
            p = p.left
        else:
            if p.right is None:
                p.right = IntTrie()
            p = p.right
        key = key>>1
    p.value = value
    return t

def lookup(t, key):
    while key != 0 and (t is not None):
        if key & 1 == 0:
            t = t.left
        else:
            t = t.right
        key = key>>1
    if t is not None:
        return t.value
    else:
        return None

def trie_to_str(t, prefix=0, depth=0):
    to_str = lambda x: "%s" %x
    str="("+to_str(prefix)
    if t.value is not None:
        str += ":"+t.value
    if t.left is not None:
        str += ", "+trie_to_str(t.left, prefix, depth+1)
    if t.right is not None:
        str += ", "+trie_to_str(t.right, (1<<depth)+prefix, depth+1)
    str+=")"
    return str

def list_to_trie(l):
    return from_list(l, trie_insert)

def map_to_trie(m):
    return from_map(m, trie_insert)

class IntTrieTest:
    def __init__(self):
        print "start trie test"

    def run(self):
        self.test_insert()
        self.test_lookup()

    def test_insert(self):
        t = None
        t = trie_insert(t, 0);
        t = trie_insert(t, 1);
        t = trie_insert(t, 4);
        t = trie_insert(t, 5);
        print trie_to_str(t)
        t1 = list_to_trie([1, 4, 5])
        print trie_to_str(t1)
        t2 = map_to_trie({4:'b', 1:'a', 5:'c', 9:'d'})
        print trie_to_str(t2)

    def test_lookup(self):
        t = map_to_trie({4:'y', 1:'x', 5:'z'})
        print "look up 4: ", lookup(t, 4)
        print "look up 5: ", lookup(t, 5)
        print "look up 0: ", lookup(t, 0)

if __name__ == "__main__":
    IntTrieTest().run()

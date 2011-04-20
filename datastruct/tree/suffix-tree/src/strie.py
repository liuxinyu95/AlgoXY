#!/usr/bin/python

# strie.py, Suffix Trie.
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

class STrie:
    def __init__(self, suffix=None):
        self.children = {}
        self.suffix = suffix

def insert(top, c):
    if top is None:
        top=STrie()
    node = top
    new_node = STrie() #dummy init value
    while (node is not None) and (c not in node.children):
        new_node.suffix = node.children[c] = STrie(node)
        new_node = node.children[c]
        node = node.suffix
    if node is not None:
        new_node.suffix = node.children[c]
    return top.children[c] #update top

def root(node):
    while node.suffix is not None:
        node = node.suffix
    return node

def lookup(t, str):
    for c in str:
        if c not in t.children:
            return None
        t = t.children[c]
    return t

def suffix_trie(str):
    t = None
    for c in str:
        t = insert(t, c)
    return root(t)

def to_lines(t):
    if len(t.children)==0:
        return [""]
    res = []
    for c, tr in sorted(t.children.items()):
        lines = to_lines(tr)
        lines[0] = "|--"+c+"-->"+lines[0]
        if len(t.children)>1:
            lines[1:] = map(lambda l: "|      "+l, lines[1:])
        else:
            lines[1:] = map(lambda l: "       "+l, lines[1:])
        if res !=[]:
            res.append("|")
        res += lines
    return res

def to_str(t):
    return "\n".join(to_lines(t))

class SuffixTrieTest:
    def __init__(self):
        print "start suffix trie test"

    def run(self):
        self.test_build()
        self.test_lookup()

    def __test_build(self, str):
        print "Suffix Trie ("+str+"):\n", to_str(suffix_trie(str)),"\n"

    def test_build(self):
        str="cacao"
        for i in range(len(str)):
            self.__test_build(str[:i+1])

    def test_lookup(self):
        str="cacao"
        t = suffix_trie(str)
        for i in range(len(str)):
            print "lookup "+str[i:]+":", lookup(t, str[i:])!=None
        print "lookup oac: ", lookup(t, "oac")!=None

if __name__ == "__main__":
    SuffixTrieTest().run()

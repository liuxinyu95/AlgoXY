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

class Node:
    def __init__(self, suffix=None):
        self.children = {} # 'c':(word, Node), where word = (l, r)
        self.suffix = suffix

class STree:
    def __init__(self, s):
        self.str = s
        self.infinity = len(s)+1000
        self.root = Node()

    def substr(self, sref):
        return substr(self.str, sref)

#
# In order to improve the efficiency by operation on-line
# There is only one copy of the string. All sub strings are
# represented as reference pair: 
#   w = (k, p)
# where: w = str[k:p+1], +1 is because of Python's specific
# problem
#
def substr(str, str_ref):
    (l, r)=str_ref
    return str[l:r+1]

def length(str_ref):
    (l, r)=str_ref
    return r-l+1

#
# Algorithm 2 in Ukkonen '95
#   l: left index of a word, k in Ukkonen '95
#   node: s in Ukkonen '95
#   None: _|_ in Ukkonen '95
#
def suffix_tree(str):
    t = STree(str)
    node = t.root # init active point is (root, Empty)
    l = 0
    for i in range(len(str)):
        (node, l) = update(t, node, (l, i))
        (node, l) = canonize(t, node, (l, i))
    return t

#
# Main func: STree(Str[i-1]) ==> STree(Str[i])
#   param: (node, (l, i-1)): AP (Active Point)
#   prev: oldr in Ukkonen '95
#   p:    r in Ukkonen '95
#   return, EP (End Point) ref pair (node, (l, i-1))
#
def update(t, node, str_ref):
    (l, i) = str_ref 
    c = t.str[i] # current char
    prev = Node() # dummy init 
    while True:
        (finish, p) = branch(t, node, (l, i-1), c)
        if finish:
            break
        p.children[c]=((i, t.infinity), Node())
        prev.suffix = p
        prev = p
        # go up along suffix link
        (node, l) = canonize(t, node.suffix, (l, i-1))
    prev.suffix = node
    return (node, l)

#
# branch: 
#  this is test-and-split function in Ukkonen '95
#  test if (node, str_ref) is an EP, (EP has a c-transition)
#  if not, then branch out a new node
#
def branch(t, node, str_ref, c):
    (l, r) = str_ref
    if length(str_ref)<=0: # (node, empty)
        if node is None: #_|_
            return (True, t.root)
        else:
            return ((c in node.children), node)
    else:
        ((l1, r1), node1) = node.children[t.str[l]]
        pos = l1+length(str_ref)
        if t.str[pos]==c:
            return (True, node)
        else:             # node--->branch_node--->node1
            branch_node = Node()
            node.children[t.str[l1]]=((l1, pos-1), branch_node)
            branch_node.children[t.str[pos]] = ((pos, r1), node1)
            return (False, branch_node)

#
# node[c]--->(l, r), _ 
# node[c]--->((l', r'), node')--->...-->((l'', r''), node'')--->((x, inf), leaf)
# where _: it may not be a node, but some implicity position
# find the closet node and left, so that they point to same position _
# special case: ||None, (k, p)|| = (root, (k+1, p))
#
def canonize(t, node, str_ref):
    (l, r) = str_ref 
    if node is None:
        if length(str_ref)<=0:
            return (None, l)
        else:
            return canonize(t, t.root, (l+1, r))
    while l<=r: # str_ref is not empty
        ((l1, r1), child) = node.children[t.str[l]] # node--(l', r')-->child
        if r-l >= r1-l1: #node--(l',r')-->child--->...
            l += r1-l1+1 # remove |(l',r')| chars from (l, r)
            node = child 
        else:
            break
    return (node, l)

def to_lines(t, node):
    if len(node.children)==0:
        return [""]
    res = []
    for c, (str_ref, tr) in sorted(node.children.items()):
        lines = to_lines(t, tr)
        edge_str = substr(t.str, str_ref)
        lines[0] = "|--"+edge_str+"-->"+lines[0]
        if len(node.children)>1:
            lines[1:] = map(lambda l: "|"+" "*(len(edge_str)+5)+l, lines[1:])
        else:
            lines[1:] = map(lambda l: " "+" "*(len(edge_str)+6)+l, lines[1:])
        if res !=[]:
            res.append("|")
        res += lines
    return res

def to_str(t):
    return "\n".join(to_lines(t, t.root))

class SuffixTreeTest:
    def __init__(self):
        print "start suffix tree test"

    def run(self):
        strs = ["cacao", "mississippi", "banana$"]
        for s in strs:
            self.test_build(s)

    def test_build(self, str):
        for i in range(len(str)):
            self.__test_build(str[:i+1])


    def __test_build(self, str):
        print "Suffix Tree ("+str+"):\n", to_str(suffix_tree(str)),"\n"

if __name__ == "__main__":
    SuffixTreeTest().run()

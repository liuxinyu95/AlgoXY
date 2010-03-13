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

# In order to improve the efficiency by operation on-line
# There is only one copy of the string. All sub strings are
# represented as reference pair: 
#   w = (k, p)
# where: w = str[k:p+1], +1 is because of Python's specific
# problem
def substr(str, left, right):
    return str[left:right+1]

# left: k in Ukkonen '95
# node: s in Ukkonen '95
def suffix_tree(str):
    t = STree(str)
    bottom = Node()

    for i in range(len(str)):
        bottom.children[str[i]]=((i, i), t.root)
    t.root.suffix = bottom

    node = t.root # init active point is root
    left = 0
    for i in range(len(str)):
        print "i=", i, "left=", left
        (node, left) = update(t, node, (left, i))
        print "after update, left=", left
        if node == t.root:
            print "after update, node=root"
        if node == bottom:
            print "after update, node = bottom"
        (node, left) = canonize(t, node, (left, i))
        print "after canonize, left=", left
        if node == t.root:
            print "after canonize, node = root"
        if node == bottom:
            print "after canonize, node = bottom"
    return t

# Main func: STree(Str[i-1]) ==> STree(Str[i])
# prev: oldr in Ukkonen '95
# p:    r in Ukkonen '95
# @return, end-point ref pair (s_j', (left, i-1))
def update(t, node, str_ref):
    (left, i) = str_ref 
    c = t.str[i] # current char
    # (node, (left, right-1)), canonical ref pair for the active point
    prev = t.root # dummy init value
    (finish, p) = branch(t, node, (left, i-1), c)
    while not finish:
        p.children[c]=((i, t.infinity), Node())
        if prev != t.root:
            prev.suffix = p

        prev = p
        (node, left) = canonize(t, node.suffix, (left, i-1))
        print "    inside update, left=", left
        (finish, p) = branch(t, node, (left, i-1), c)
    if prev != t.root:
        prev.suffix = node
    return (node, left)

# branch: 
#  test if (node, str_ref) has c-transition already
#  if not, then branch out a new node
# case 1:
#   (root, empty), c ==> root.children[c] is None?
# case 2:
#   (root, (0, i-1)), c ==> add c to top, always return true, root
# case 3:
#
def branch(t, node, str_ref, c):
    (l, r) = str_ref
    if l <= r:
        ((l1, r1), node1) = node.children[t.str[l]]
        print "    l=", l, "r=", r, "l1=", l1, "r1=", r1
        print "    str[tk+offset+1]=", t.str[l1+r-l+1], "c=", c
        if t.str[l1+(r-l+1)]==c:
            return (True, node)
        else:
            # node--->branch_node--->node1
            branch_node = Node()
            pos = l1+r-l
            node.children[t.str[l1]]=((l1, pos), branch_node)
            branch_node.children[t.str[pos+1]] = ((pos+1, r1), node1)
            return (False, branch_node)
    else:
        return (c in node.children, node)

# node[c]--->(l, r), _ 
# node[c]--->((l', r'), node')--->...-->((l'', r''), node'')--->((x, inf), leaf)
# where _: it may not be a node, but some implicity position
# find the closet node and left, so that they point to same position _
def canonize(t, node, str_ref):
    (l, r) = str_ref 
    print "    canonize: l=", l, "r=", r
    while l<=r: # str_ref is not empty
        ((l1, r1), child) = node.children[t.str[l]] # node--(l', r')-->child
        print "        str[l]=", t.str[l], "l1=", l1, "r1=", r1
        if r-l >= r1-l1: #node--(l',r')-->child--->...
            l += r1-l1+1 # remove |(l',r')| chars from (l, r)
            node = child 
        else:
            break
    print "    canonized: l=", l
    return (node, l)

class SuffixTreeTest:
    def __init__(self):
        print "start suffix tree test"

    def run(self):
        self.test_build()

    def test_build(self):
        str="cacao"
        #for i in range(len(str)):
        #    self.__test_build(str[:i+1])
        suffix_tree(str)

    def __test_build(self, str):
        print "build suffix tree(", str, ")"
        suffix_tree(str)

if __name__ == "__main__":
    SuffixTreeTest().run()

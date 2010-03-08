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
        self.children = {}
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

def infinity(str):
    return len(str)+1000

# left: k in Ukkonen '95
# node: s in Ukkonen '95
def suffix_tree(str):
    t = STree(str)
    node = t.root # init active point is root
    left = 0
    for i in range(len(str)):
        (node, left) = update(t, node, (left, i))
        (node, left) = canonize(t, node, (left, i))
    return t

# prev: oldr in Ukkonen '95
# p:    r in Ukkonen '95
def update(t, node, str_ref):
    (left, i) = str_ref 
    # (node, (left, right-1)), canonical ref pair for the active point
    prev = Node() # dummy init value
    (is_end_point, p) = branch(t, node, (left, i-1), t.str[i])
    while not is_end_point:
        p.children[t.str[i]]=((i, t.infinity), Node())
        prev.suffix = p
        prev = p
        (node, k) = canonize(t, node.suffix, (k, i-1))
        (is_end_point, p) = branch(t, node, (k, i-1), t.str[i])
    if prev != t.root:
        prev.suffix = node
    return (node, k)

# branch: test-and-split in Ukkonen '95
#  test if (node, str_ref) is the end point
#  if not, then branch out a new node
def branch(t, node, str_ref, c):
    (left, right) = str_ref
    if left <= right:
        ((left1, right1), node1) = node.children[t.str[left]]
    else:
        return (c in node.children, node)
            
def canonize(node, str_ref):
    if node is None:
        pass
    (left, right) = str_ref
    if

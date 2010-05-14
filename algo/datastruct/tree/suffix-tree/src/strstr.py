#!/usr/bin/python

# strstr.py, String Manipulation over suffix tree
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
import stree

# special terminators
TERM1 = '$'
TERM2 = '#'

# find if a how many occurrence of a substring
# the algorithm is similar to the standard patricia one.
# so I use the recursive version to simplify the logic for
# illustration purpose
def lookup_pattern(t, node, s):
    f = (lambda x: 1 if x==0 else x)
    for _, (str_ref, tr) in node.children.items():
        edge = t.substr(str_ref)
        if string.find(edge, s)==0: #s `isPrefixOf` edger
            return f(len(tr.children))
        elif string.find(s, edge)==0: #edge `isPrefixOf` s
            return lookup_pattern(t, tr, s[len(edge):])
    return 0 # not found

# search the longest repeating substringS
#  BFS approach
def lrs(t):
    queue = [("", t.root)]
    res = []
    while len(queue)>0:
        (s, node) = queue.pop(0)
        for _, (str_ref, tr) in node.children.items():
            if len(tr.children)>0:
                s1 = s+t.substr(str_ref)
                queue.append((s1, tr))
                res = update_max(res, s1)
    return res

def update_max(lst, x):
    if lst ==[] or len(lst[0]) < len(x):
        return [x]
    elif len(lst[0]) == len(x):
        return lst + [x]
    else:
        return lst

# search the longest common substring
#  t = SuffixTree(s1#s2$)
def lcs(t):
    pass

class STreeUtil:
    def __init__(self):
        self.tree = None

    def __lazy__(self, str):
        if self.tree is None or self.tree.str!=str+TERM1:
            self.tree = stree.suffix_tree(str+TERM1)

    def find_pattern(self, str, pattern):
        self.__lazy__(str)
        return lookup_pattern(self.tree, self.tree.root, pattern)

    def find_lrs(self, str):
        self.__lazy__(str)
        return lrs(self.tree)

class StrSTreeTest:
    def __init__(self):
        print "start string manipulation over suffix tree test"
        self.util=STreeUtil()

    def run(self):
        self.test_find_pattern()
        self.test_lrs()

    def test_find_pattern(self):
        self.__test_pattern__("banana", "ana")
        self.__test_pattern__("banana", "an")
        self.__test_pattern__("banana", "anan")
        self.__test_pattern__("banana", "nana")
        self.__test_pattern__("banana", "ananan")

    def __test_pattern__(self, s, p):
        print "find pattern", p, "in", s, ":", self.util.find_pattern(s, p)

    def test_lrs(self):
        self.__test_lrs__("mississippi")
        self.__test_lrs__("banana")
        self.__test_lrs__("cacao")
        self.__test_lrs__("foofooxbarbar")

    def __test_lrs__(self, s):
        print "longest repeated substrings of", s, "=", self.util.find_lrs(s)

if __name__ == "__main__":
    StrSTreeTest().run()

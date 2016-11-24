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
def lookup_pattern(t, node, s):
    while True:
        match = False
        for _, (str_ref, tr) in node.children.items():
            edge = t.substr(str_ref)
            if string.find(edge, s)==0: #s `isPrefixOf` edge
                return max(len(tr.children), 1)
            elif string.find(s, edge)==0: #edge `isPrefixOf` s
                match = True
                node = tr
                s = s[len(edge):]
                break
        if not match:
            return 0
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
    if len(lst[0]) == len(x):
        return lst + [x]
    return lst

# search the longest common substring
#  t = SuffixTree(s1#s2$)
def lcs(t):
    queue = [("", t.root)]
    res = []
    while len(queue)>0:
        (s, node) = queue.pop(0)
        if match_fork(t, node):
            res = update_max(res, s)
        for _, (str_ref, tr) in node.children.items():
            if len(tr.children)>0:
                s1 = s + t.substr(str_ref)
                queue.append((s1, tr))
    return res

def is_leaf(node):
    return node.children=={}

def match_fork(t, node):
    if len(node.children)==2:
        [(_, (str_ref1, tr1)), (_, (str_ref2, tr2))]=node.children.items()
        return is_leaf(tr1) and is_leaf(tr2) and \
            (t.substr(str_ref1).find(TERM2)!=-1) != \
            (t.substr(str_ref2).find(TERM2)!=-1)
    return False

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

    def find_lcs(self, s1, s2):
        self.__lazy__(s1+TERM2+s2)
        return lcs(self.tree)

    def find_lpalindrome(self, s):
        return self.find_lcs(s, s[::-1]) #l[::-1] = reverse(l)

class StrSTreeTest:
    def __init__(self):
        print "start string manipulation over suffix tree test"
        self.util=STreeUtil()

    def run(self):
        self.test_find_pattern()
        self.test_lrs()
        self.test_lcs()
        self.test_lpalindrome()

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

    def test_lcs(self):
        self.__test_lcs__("ababa", "baby")
        self.__test_lcs__("ff", "bb")

    def __test_lcs__(self, s1, s2):
        print "longest common substring of", s1, "and", s2, "=", self.util.find_lcs(s1, s2)

    def test_lpalindrome(self):
        self.__test_lpalindrome__("mississippi")
        self.__test_lpalindrome__("banana")
        self.__test_lpalindrome__("cacao")
        self.__test_lpalindrome__("Woolloomooloo")
        self.__test_lpalindrome__("abacoxyocaba"); #false case so far.

    def __test_lpalindrome__(self, s):
        print "longest palindrome of", s, "=", self.util.find_lpalindrome(s)

if __name__ == "__main__":
    StrSTreeTest().run()

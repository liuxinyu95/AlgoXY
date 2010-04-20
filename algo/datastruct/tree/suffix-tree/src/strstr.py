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
    f = (lambda x: x==0 and 1 or x)
    for _, (str_ref, tr) in node.children.items():
        edge = t.substr(str_ref)
        if string.find(edge, s)==0: #s `isPrefixOf` edge
            return f(len(tr.children))
        elif string.find(s, edge)==0: #edge `isPrefixOf` s
            return lookup_pattern(t, tr, s[len(edge):])
    return 0 # not found

class STreeUtil:
    def __init__(self):
        self.tree = None

    def find_pattern(self, str, pattern):
        if self.tree is None or self.tree.str!=str+TERM1:
            self.tree = stree.suffix_tree(str+TERM1)
        return lookup_pattern(self.tree, self.tree.root, pattern)

class StrSTreeTest:
    def __init__(self):
        print "start string manipulation over suffix tree test"

    def run(self):
        self.test_find_pattern()

    def test_find_pattern(self):
        util=STreeUtil()
        self.__test_pattern__(util, "banana", "ana")
        self.__test_pattern__(util, "banana", "an")
        self.__test_pattern__(util, "banana", "anan")
        self.__test_pattern__(util, "banana", "nana")
        self.__test_pattern__(util, "banana", "ananan")

    def __test_pattern__(self, u, s, p):
        print "find pattern", p, "in", s, ":", u.find_pattern(s, p)

if __name__ == "__main__":
    StrSTreeTest().run()

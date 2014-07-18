#!/usr/bin/python

# edict.py, E-dictionary and T9 input method based on Patricia and Trie.
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
import trie
import patricia

# expand all sub branches with a prefix string
def expand(prefix, t, n):
    res = []
    q = [(prefix, t)]
    while len(res)<n and len(q)>0:
        (s, p) = q.pop(0)
        if p.value is not None:
            res.append((s, p.value))
        for k, tr in p.children.items():
            q.append((s+k, tr))
    return res

# lookup top N candidate start from key
def trie_lookup(t, key, n):
    if t is None:
        return None

    p = t
    for c in key:
        if not c in p.children:
            return None
        p=p.children[c]
    return expand(key, p, n)

def patricia_lookup(t, key, n):
    if t is None:
        return None
    prefix = ""
    while True:
        match = False
        for k, tr in t.children.items():
            if string.find(k, key) == 0: #is prefix of
                return expand(prefix+k, tr, n)
            if string.find(key, k) ==0:
                match = True
                key = key[len(k):]
                t = tr
                prefix += k
                break
        if not match:
            return None

T9MAP={'2':"abc", '3':"def", '4':"ghi", '5':"jkl", \
       '6':"mno", '7':"pqrs", '8':"tuv", '9':"wxyz"}

def trie_lookup_t9(t, key):
    if t is None or key == "":
        return None
    q = [("", key, t)]
    res = []
    while len(q)>0:
        (prefix, k, t) = q.pop(0)
        i=k[0]
        if not i in T9MAP:
            return None #invalid input
        for c in T9MAP[i]:
            if c in t.children:
                if k[1:]=="":
                    res.append((prefix+c, t.children[c].value))
                else:
                    q.append((prefix+c, k[1:], t.children[c]))
    return res

def toT9(s):
    res=""
    for c in s:
        for k, v in T9MAP.items():
            if string.find(v, c)>=0:
                res+=k
                break
        #error handling skipped.
    return res

def patricia_lookup_t9(t, key):
    if t is None or key == "":
        return None
    q = [("", key, t)]
    res = []
    while len(q)>0:
        (prefix, key, t) = q.pop(0)
        for k, tr in t.children.items():
            digits = toT9(k)
            if string.find(key, digits)==0: #is prefix of
                if key == digits:
                    res.append((prefix+k, tr.value))
                else:
                    q.append((prefix+k, key[len(k):], tr))
    return res

class LookupTest:
    def __init__(self):
        print "word auto-completion and T9 test"
        dict = {"a":"the first letter of English", \
           "an":"used instead of 'a' when the following word begins with a vowel sound", \
           "another":"one more person or thing or an extra amount", \
           "abandon":"to leave a place, thing or person forever",\
           "about":"on the subject of; connected with",\
           "adam":"a character in the Bible who was the first man made by God",\
           "boy":"a male child or, more generally, a male of any age", \
           "body":"the whole physical structure that forms a person or animal", \
           "zoo":"an area in which animals, especially wild animals, are kept so that people can go and look at them, or study them"}
        self.tt = trie.map_to_trie(dict)
        self.tp = patricia.map_to_patricia(dict)
        t9dict = ["home", "good", "gone", "hood", "a", "another", "an"]
        self.t9t = trie.list_to_trie(t9dict)
        self.t9p = patricia.list_to_patricia(t9dict)

    def run(self):
        self.test_trie_lookup()
        self.test_patricia_lookup()
        self.test_trie_t9()
        self.test_patricia_t9()

    def test_trie_lookup(self):
        print "test lookup top 5 in Trie"
        print "search a ", trie_lookup(self.tt, "a", 5)
        print "search ab ", trie_lookup(self.tt, "ab", 5)

    def test_patricia_lookup(self):
        print "test lookup top 5 in Patricia"
        print "search a ", patricia_lookup(self.tp, "a", 5)
        print "search ab ", patricia_lookup(self.tp, "ab", 5)

    def test_trie_t9(self):
        print "test t9 lookup in Trie"
        print "search 4 ", trie_lookup_t9(self.t9t, "4")
        print "search 46 ", trie_lookup_t9(self.t9t, "46")
        print "search 4663 ", trie_lookup_t9(self.t9t, "4663")
        print "search 2 ", trie_lookup_t9(self.t9t, "2")
        print "search 22 ", trie_lookup_t9(self.t9t, "22")

    def test_patricia_t9(self):
        print "test t9 lookup in Patricia"
        print "search 4 ", patricia_lookup_t9(self.t9p, "4")
        print "search 46 ", patricia_lookup_t9(self.t9p, "46")
        print "search 466 ", patricia_lookup_t9(self.t9p, "466")
        print "search 4663 ", patricia_lookup_t9(self.t9p, "4663")
        print "search 2 ", patricia_lookup_t9(self.t9p, "2")
        print "search 22 ", patricia_lookup_t9(self.t9p, "22")

if __name__ == "__main__":
    LookupTest().run()

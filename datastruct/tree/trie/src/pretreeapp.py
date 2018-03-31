#!/usr/bin/python

# pretreeapp.py, Prefix tree applications,
# including E-dictionary and T9 input method
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
import prefixtree

# returns up to n candidates start with given prefix
def lookup(t, key, n):
    if t is None:
        return []
    prefix = ""
    while True:
        match = False
        for k, tr in t.subtrees.items():
            if string.find(k, key) == 0: # key is prefix of k
                return expand(prefix + k, tr, n)
            if string.find(key, k) ==0:
                match = True
                key = key[len(k):]
                t = tr
                prefix += k
                break
        if not match:
            break
    return []

# expand all sub branches with a prefix string
def expand(prefix, t, n):
    res = []
    q = [(prefix, t)]
    while len(res)<n and q:
        (s, p) = q.pop(0)
        if p.value is not None:
            res.append((s, p.value))
        for k, tr in sorted(p.subtrees.items()):
            q.append((s + k, tr))
    return res

T9MAP={'2':"abc", '3':"def", '4':"ghi", '5':"jkl", \
       '6':"mno", '7':"pqrs", '8':"tuv", '9':"wxyz"}

T9RMAP = dict([(c, d) for d, cs in T9MAP.items() for c in cs])

def digits(w):
    return ''.join([T9RMAP[c] for c in w])

def lookup_t9(t, key):
    if t is None or key == "":
        return []
    res = []
    n = len(key)
    q = [("", key, t)]
    while q:
        prefix, key, t = q.pop(0)
        for k, tr in t.subtrees.items():
            ds = digits(k)
            if string.find(ds, key) == 0: # key is prefix of ds
                res.append((prefix + k)[:n])
            elif string.find(key, ds) == 0: # ds is prefix of key
                q.append((prefix + k, key[len(k):], tr))
    return res

def test_edict():
    m = {"a":"the first letter of English", \
         "an":"used instead of 'a' when the following word begins with a vowel sound", \
         "another":"one more person or thing or an extra amount", \
         "abandon":"to leave a place, thing or person forever",\
         "about":"on the subject of; connected with",\
         "adam":"a character in the Bible who was the first man made by God",\
         "boy":"a male child or, more generally, a male of any age", \
         "body":"the whole physical structure that forms a person or animal", \
         "zoo":"an area in which animals, especially wild animals, are kept so that people can go and look at them, or study them"}
    t = prefixtree.from_map(m)
    verify_lookup(m, t, "a", 5)
    verify_lookup(m, t, "a", 6);
    verify_lookup(m, t, "a", 7);
    verify_lookup(m, t, "ab", 2);
    verify_lookup(m, t, "ab", 5);
    verify_lookup(m, t, "b", 2);
    verify_lookup(m, t, "bo", 5);
    verify_lookup(m, t, "z", 3);
    print "lookup with given prefix tested."

def verify_lookup(m, t, key, n):
    r1 = sorted(lookup(t, key, n))
    r2 = sorted([(k, v) for k, v in m.items() if string.find(k, key) == 0])[:n]
    assert(r1 == r2)

def test_t9():
    ws = ["home", "good", "gone", "hood", "a", "another", "an"]
    t = prefixtree.from_text(' '.join(ws))
    for ns in ["4663", "22", "2668437"]:
        for i in range(1, len(ns)):
            ds = ns[:i]
            r1 = set(lookup_t9(t, ds))
            r2 = set([w[:i] for w in ws if digits(w)[:i] == ds])
            assert(r1 == r2)
    print "T9 tested."

if __name__ == "__main__":
    test_edict()
    test_t9()

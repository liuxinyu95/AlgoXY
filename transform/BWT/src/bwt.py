#! /usr/bin/python

# bwt.py, Burrows-Wheeler transformation
# Copyright (C) 2011 Liu Xinyu (liuxinyu95@gmail.com)
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

EOF = "\0"

# Algorithm 1, Naive version from [1]

def bwt(s):
    s += EOF
    table = sorted(s[i:] + s[:i] for i in range(len(s)))
    last_colum = [row[-1] for row in table]
    return "".join(last_colum)

def ibwt(r):
    table = [""] * len(r)
    for i in range(len(r)):
        table = sorted(r[i] + table[i] for i in range(len(r)))
    s = [row for row in table if row.endswith(EOF)][0]
    return s.rstrip(EOF)

# Algorithm 2, 
# Instead of sorting on rotation, we can sort on suffixes by 
# adding a special EOF char. [2].

# example: s = apple, EOF = $ (== -inf)
#
# suffixes       rotations
#  apple$        apple$
#   pple$        pple$a
#    ple$        ple$ap
#     le$        le$app
#      e$        e$appl
#       $        $apple
#
#         sorting
#
#     let i = len(s) - len(suffix)
#            i s[i-1]
#  $         5   e  $apple
#  apple$    0   $  apple$
#  e$        4   l  e$appl
#  le$       3   p  le$app
#  ple$      2   p  ple$ap
#  pple$     1   a  pple$a
#
#  result: e$lppa

def bwt1(s):
    s += EOF
    l = len(s)
    table = sorted(s[i:] for i in range(l))
    last_colum = [s[l-len(row)-1] for row in table]
    return "".join(last_colum)

# Algorithm 3,
# Improvement 1: Instead of using explicit EOF, we can provide 
#                a customized compare funciton for sorting. [3]
# Improvement 2: Instead of using instance of suffix strings, 
#                using suffix indecs. to save spaces.

# fact:
#   let 
#     s' = s + EOF
#     table = sort(suffixes(s'))
#     
#   we have: for every row in table,
#     index of row[0] in s' == len(s) - len(row)

def bwt2(s):
    ids = sorted(range(len(s)+1), lambda x, y: scmp(s, x, y))
    last_colum = [s[i-1] for i in ids]
    ifst = ids.index(1) # the position of s[0] in last_colum
    ieof = ids.index(0) # EOF, which is s[-1] virtually
    return ("".join(last_colum), ifst, ieof)

# suffixes comparison function, which can handle virtual EOF
#   and we treat EOF as -inf
def scmp(s, i, j):
    x = s[i:]
    y = s[j:]
    if x == []: # x is EOF
        return -1
    if y == []: # y is EOF
        return 1
    return cmp(x, y)

def ibwt2(t):
    (r, ifst, ieof) = t
    fst_colum = [r[ieof]]+sorted(r[:ieof]+r[ieof+1:]) # first element is EOF
    ids = [False]+[True]*(len(r)-1)
    trans=[0]*len(r)  # transform vector
    for i in range(len(r)):
        if i != ieof:
            x = [j for j in range(len(r)) if ids[j] and fst_colum[j] == r[i]][0]
            trans[x] = i
            ids[x] = False
    s = ""
    i = ifst
    for j in range(len(r)-1):
        s = s+r[i]
        i = trans[i]
    return s

# Algorithm 4,
#   Same as algorithm 3, but we sort on rots index
#   The main improvement is in inverse BWT, the transform
#   vector can be calculated as the following
#
#   T = snd $ sort (zip(r, [1..])
#
def bwt3(s):
    n = len(s)
    ids = sorted(range(n), lambda x, y: rcmp(s, x, y))
    last_colum = [s[(i-1)%n] for i in ids]
    return ("".join(last_colum), ids.index(0))

def rcmp(s, i, j):
    x = s[i:]+s[:i]
    y = s[j:]+s[:j]
    return cmp(x, y)

def ibwt3(p):
    (r, i) = p
    n = len(r)
    trans = [ x for (c, x) in sorted(zip(r, range(n)))]
    s = ""
    for _ in range(n):
        i = trans[i]
        s = s + r[i]
    return s

def test():
    s = "this is the demo program of bwt transform in the real programming code"
    r = bwt(s)
    print "s=", s
    print "bwt(s) ==>", r
    print "ibwt(r) ==>", ibwt(r)
    print "bwt1(s) ==>", bwt1(s)
    print "ibwt(bwt1(s)) ==>", ibwt(bwt1(s))
    t = bwt2(s)
    print "bwt2(s) ==>", t
    print "ibwt2(r) ==>", ibwt2(t)
    p = bwt3(s)
    print "bwt3(s) ==>", p
    print "ibwt3(r) ==>", ibwt3(p)

if __name__ == "__main__":
    test()

# reference
# [1], WIKI. http://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
# [2], M. Burrows and D.J. Wheeler `A Block-sorting Loseless Data Compression Algorithm'. DEC Research center, SRC research report, May 10, 1994
# [3], Mark Nelson. `Data Compression with the Burrows-Wheeler Transform'. http://marknelson.us/1996/09/01/bwt/

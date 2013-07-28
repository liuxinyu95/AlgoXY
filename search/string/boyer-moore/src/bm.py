#!/usr/bin/python

# Boyer-Moore string matching algorithms

# For illustration purpose, assume charset is ASCII only.

import random # for verification purpose only
import string # for verification purpose only

# Boyer-Moore variant 1
#  Boyer-Moore-Horspool algorithm
#  This is a simplified version, which only utilized bad-character rule

#  The bad-character rule should ONLY be used for the position aligned
#  with the LAST character in the pattern no matter where the mismatch
#  happens

# Match pattern p in text w
def bmh_match(w, p):
    n = len(w)
    m = len(p)
    tab = bad_char(p)
    res = []
    offset = 0
    while offset + m <= n:
        i = m - 1
        while i >= 0 and p[i] == w[offset+i]:
            i = i - 1
        if i < 0:
            res.append(offset)
            offset = offset + 1
        else:
            offset = offset + tab[ord(w[offset + m - 1])]
    return res

# The bad-character rulea
#  Note that, the calculation for the last character in the pattern is skipped,
#  This is becasue we want to avoid 0 shifting, which will lead to endless loop.
def bad_char(p):
    m = len(p)
    tab = [m for _ in range(256)]  # table to hold the bad character rule.
    for i in range(m-1):
        tab[ord(p[i])] = m - 1 - i
    return tab

# Boyer-Moore original algorithm
#   Use both the bad-character rule, and the good-suffix rule

def bm_match(w, p):
    n = len(w)
    m = len(p)
    tab1 = bad_char(p)
    tab2 = good_suffix(p)
    res = []
    offset = 0
    while offset + m <= n:
        i = m - 1
        while i >= 0 and p[i] == w[offset + i]:
            i = i - 1
        if i < 0:
            res.append(offset)
            offset = offset + 1
        else:
            offset = offset + max(tab1[ord(w[offset + m - 1])], tab2[i])
    return res

# The good-suffix rule
#   case 1. Only a part of the matching suffix occurs as a prefix of the pattern
#   case 2. The matching suffix occurs somewhere else in the pattern
#  Note that, in case 1, we skip the empty suffix case.

def good_suffix(p):
    m = len(p)
    tab = [0 for _ in range(m)]
    last = 0
    # first loop for case 1
    for i in range(m-1, 0, -1): # m-1, m-2, ..., 1
        if is_prefix(p, i):
            last = i
        tab[i - 1] = last
    # second loop for case 2
    for i in range(m):
        slen = suffix_len(p, i)
        if slen != 0 and p[i - slen] != p[m - 1 - slen]:
            tab[m - 1 - slen] = m - 1 - i
    return tab

# test if p[i..m-1] `is prefix of` p
def is_prefix(p, i):
    for j in range(len(p) - i):
        if p[j] != p [i+j]:
            return False
    return True

# length of the longest suffix of p[..i], which is also a suffix of p
def suffix_len(p, i):
    m = len(p)
    j = 0
    while p[m - 1 - j] == p[i - j] and j < i:
        j = j + 1
    return j

# naive search for verification
#   O(m*n) algorithm
def naive_match(w, p):
    n = len(w)
    m = len(p)
    res = []
    for i in range(n):
        if w[i:(i+m)] == p:
            res.append(i)
    return res

def test(match):
    for k in xrange(100):
        n = random.randint(10, 10000)
        w = [random.choice(string.ascii_lowercase) for i in xrange(n)]
        i = random.randint(0, n-2)
        j = random.randint(i+1, n)
        p = w[i:j]
        __assert(match(w, p), naive_match(w, p))
    print "100 test cases run OK"

def __assert(x, y):
    if x!=y:
        print "left:", x, "right:", y

if __name__ == "__main__":
    print "boyer-moore-horspool"
    #print bmh_match("bba", "a")
    #print bmh_match("bbaabab", "ab")
    test(bmh_match)
    print "boyer-moore"
    #print bm_match("bba", "a")
    #print bm_match("bbaabab", "ab")
    #print bm_match("bbbababbababbabababb", "abbabab")
    test(bm_match)

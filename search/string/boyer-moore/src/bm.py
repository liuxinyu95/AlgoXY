#!/usr/bin/python

# Boyer-Moore string matching algorithms

# For illustration purpose, assume charset is ASCII only.

import random # for verification purpose only
import string # for verification purpose only

# Boyer-Moore variant 1
#  Boyer-Moore-Horspool algorithm
#  This is a simplified version, which only utilized bad-character rule

# Match pattern p in text w
def bmh_match(w, p):
    n = len(w)
    m = len(p)
    tab = [m for _ in range(256)]  # table to hold the bad character rule.
    for i in range(m-1):
        tab[ord(p[i])] = m - 1 - i
    res = []
    offset = 0
    while offset + m <= n:
        i = m - 1
        while i >= 0 and p[i] == w[offset+i]:
            i = i - 1
        if i < 0:
            res.append(offset)
        offset = offset + tab[ord(w[offset + m - 1])]
    return res

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

def test():
    for k in xrange(100):
        n = random.randint(10, 10000)
        w = [random.choice(string.ascii_lowercase) for i in xrange(n)]
        i = random.randint(0, n-2)
        j = random.randint(i+1, n)
        p = w[i:j]
        __assert(bmh_match(w, p), naive_match(w, p))
    print "100 test cases run OK"

def __assert(x, y):
    if x!=y:
        print "left:", x, "right:", y

if __name__ == "__main__":
    #print bmh_match("bba", "a")
    #print bmh_match("bbaabab", "ab")
    test()

#! /usr/bin/python

# Algorithm 1, Naive version from [1]

# using \0 as the special EOF char
def bwt(s):
    s += "\0"
    table = sorted(s[i:] + s[:i] for i in range(len(s)))
    last_colum = [row[-1:] for row in table]
    return "".join(last_colum)

def ibwt(r):
    table = [""] * len(r)
    for i in range(len(r)):
        table = sorted(r[i] + table[i] for i in range(len(r)))
    s = [row for row in table if row.endswith("\0")][0]
    return s.rstrip("\0")

def test():
    s = "this is the demo program of bwt transform in the real programming code"
    r = bwt(s)
    print "bwt(", s, ") ==>", r
    print "ibwt(", r, ") ==>", ibwt(r)

if __name__ == "__main__":
    test()

# reference
# [1] WIKI. http://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform

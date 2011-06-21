#! /usr/bin/python

# Algorithm 1, Naive version from [1]

# using \0 as the special EOF char
def bwt(s):
    s += "\0"
    table = sorted(s[i:] + s[:i] for i in range(len(s)))
    last_colum = [row[-1] for row in table]
    return "".join(last_colum)

def ibwt(r):
    table = [""] * len(r)
    for i in range(len(r)):
        table = sorted(r[i] + table[i] for i in range(len(r)))
    s = [row for row in table if row.endswith("\0")][0]
    return s.rstrip("\0")

# Algorithm 2, 
# Instead of sorting on rotation, we can sort on suffixes by 
# adding a special EOF char. [2].

# example: s = apple, EOF = $
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
#     let i = len(s) - len(suffixes)
#            i s[i-1]
#  apple$    0   $  apple$
#  e$        4   l  e$appl
#  le$       3   p  le$app
#  ple$      2   p  ple$ap
#  pple$     1   a  pple$a
#  $         5   e  $apple

def bwt1(s):
    s += "\0"  # using \0 as the special EOF char
    l = len(s)
    table = sorted(s[i:] for i in range(l))
    last_colum = [s[l-len(row)-1] for row in table]
    return "".join(last_colum)

# Algorithm 3,

def test():
    s = "this is the demo program of bwt transform in the real programming code"
    r = bwt(s)
    print "s=", s
    print "bwt(s) ==>", r
    print "ibwt(r) ==>", ibwt(r)
    print "bwt1(s) ==>", bwt1(s)
    print "ibwt(bwt1(s)) ==>", ibwt(bwt1(s))

if __name__ == "__main__":
    test()

# reference
# [1], WIKI. http://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
# [2], M. Burrows and D.J. Wheeler `A Block-sorting Loseless Data Compression Algorithm'. DEC Research center, SRC research report, May 10, 1994

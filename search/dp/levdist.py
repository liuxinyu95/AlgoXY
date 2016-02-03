#!/usr/bin/python

# Levenshtein distance
# Given two string s, and t, compute the Levenshtein edit distance for them.
# Levenshtein distance is defined as the cost to turn s to t with the
# following 3 operations
#   insert a char;
#   delete a char;
#   substitute a char.

# Dynamic programming approach
def lev(s, t):
    m, n = len(s)+1, len(t)+1
    d = [[0]*n for _ in range(m)] #d[i][j]: Levenshtein distance between s[:i] and t[:j]
    for i in range(m): #source prefix can be transfered to empty by dropping all chars
        d[i][0] = i
    for j in range(n): #target prefix can be reached from empty from empty by insering all chars
        d[0][j] = j
    for j in range(n-1):
        for i in range(m-1):
            c = 0 if s[i] == t[j] else 1
            d[i+1][j+1] = min([d[i][j+1], d[i+1][j], d[i][j]+c])
    print "s=", s, "t=", t
    print "d=", d
    return d[m-1][n-1]

if __name__ == "__main__":
    print lev("kitten", "sitting")

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
    for j in range(n): #target prefix can be reached from empty by inserting all chars
        d[0][j] = j
    for j in range(1, n):
        for i in range(1, m):
            c = 0 if s[i-1] == t[j-1] else 1
            d[i][j] = min([d[i-1][j] + 1, #deletion
                           d[i][j-1] + 1, #insertion
                           d[i-1][j-1]+c]) #substitution
    #print "d=", d
    return d[m-1][n-1]

if __name__ == "__main__":
    print "lev(kitten, sitting)=", lev("kitten", "sitting")

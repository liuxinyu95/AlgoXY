#!/usr/bin/python

# DP solution based on [1].

# assume xs is monotonically increasing
def solve(xs, s):
    low = sum([x for x in xs if x < 0])
    up  = sum([x for x in xs if x > 0])
    if s < low or s > up:
        return False
    n = len(xs)
    q = [[False]*(up-low+1)]*n
    for i in xrange(0, n):
        for j in xrange(low, up+1):
            if i == 0:
                q[0][j] = xs[0] ==j
            else:
                q[i][j] = (q[i-1][j] or (xs[i]==j) or q[i-1][j-xs[i]])
    return q[n-1][s]

if __name__ == "__main__":
    print solve([-7, -3, -2, 5, 8], 0)

# Reference
# [1]. http://en.wikipedia.org/wiki/Subset_sum_problem

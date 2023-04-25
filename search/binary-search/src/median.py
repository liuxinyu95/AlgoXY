#
# Find the simplified median for two sorted lists a, b in O(lg(m+n)) time and O(1) space.
# Where m = len(a), n = len(b).
#
# The median is defined (differently from statistics) as:
#   median(x) = x[len(x)/2], where x = merge(a, b)
#
# While in statistics:
#   median(x) = x[n/2] if odd(n) else (x[n/2 - 1] + x[n/2]) / 2.0
#
# Note: we can NOT well define median(x) = m, where m in a + b
#    abs(len([y in a + b, y < m) - len([y in a + b, y > m])) <= 1
# Because the counter example: [0, 1, 2, 3, 3, 3, 3, 3, 5], even if replace < with <=
#

from random import sample, randint

# method 1: binary search based
#
# Perform the following binary search first in a then b:
#
# Start from low = 0, high = |a|, guess i = (low + high) / 2
# For the median m, there should be total half = (|a| + |b|) / 2 elements before it.
# Since there are i elements before a[i] in a, we expect there are j = half - i elements
# before a[i] in b. We check if b[j - 1] <= a[i] <= b[j] holds. If yes, then the guess
# is the median. Otherwise, we adjust low/high to i accordingly to halve the search.
#
# [1]. MIT, Introduction to algorithm, problem set 9-1.
# [2]. Dr. Dobb's Finding the Median of Two Sorted Arrays Efficiently.

def median(a, b):
    if not a:
        return b[len(b) // 2]
    if not b:
        return a[len(a) // 2]
    idx = medof(a, b)
    return median(b, a) if idx is None else a[idx]

def medof(a, b):
    l = 0
    u = len(a)
    while l < u:
        i = (l + u) // 2
        j = (len(a) + len(b)) // 2  - i
        if j < 1 or j >= len(b):
            if (j == 0 and a[i] <= b[0]) or (j == len(b) and b[j - 1] <= a[i]):
                return i    # found
            if j >= len(b):
                l = i + 1   # too small
            else:
                u = i       # too big
        else:
            if b[j - 1]  <= a[i] and a[i] <= b[j]:
                return i    # found
            if a[i] < b[j - 1]:
                l = i + 1   # too small
            else:
                u = i       # too big
    return None

# method 2:

def test(f, n = 100):
    for _ in range(n):
        xs = [randint(0, n // 2) for _ in range(randint(2, n))]
        m = randint(0, len(xs))
        a, b = sorted(xs[:m]), sorted(xs[m:])
        c = sorted(xs)
        med1 = f(a, b)
        med2 = c[len(c) // 2]
        assert med1 == med2, f"err: median({a}, {b}): {med1}, median({c}): {med2}"
    print(f"{n} tests OK")

if __name__ == "__main__":
    test(median)

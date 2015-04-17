#!/usr/bin/python
#!/usr/bin/python

# Recursive solution

# For the two arrays A and B:

# General recursive case:
#   Calculate the medians m1 and m2 of the input arrays A and B respectively.
#   If m1 = m2:
#      Found, return either m1 or m2.
#   If m1 < m2:
#      Recursive find the median of two sub-arrays: A[m1...] and B[...m2]
#   Otherwise, m2 < m1:
#      Recursive find the median of two sub-arrays: A[...m1] and B[m2...]

# Edge termination case
#   Both sub-arrays contain 2 elements foreach.
#     return (max(a1, b1) + min(a2, b2)) / 2

# Other handling:
#   Either array contains less than 2 elements or empty.

def median(xs, ys):
    if not xs:
        return med(ys)
    if not ys:
        return med(xs)

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

def median(xs, ys):
    if not xs:
        return med(ys)
    if not ys:
        return med(xs)

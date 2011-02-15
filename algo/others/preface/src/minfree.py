#!/usr/bin/python

def min_free(lst):
    n = len(lst)
    a = [0]*n
    for x in lst:
        if x < n:
            a[x] = 1
    return a.index(0)

def brute_force(lst):
    i = 0
    while True:
        if i not in lst:
            return i
        i = i + 1

def dc_min_free(lst):
    return binary_search(lst, 0, len(lst)-1)

def binary_search(lst, l, u):
    if lst == []:
        return l
    m = (l + u ) / 2
    xs = [x for x in lst if x <= m]
    ys = [x for x in lst if x > m]
    if len(xs) == m - l + 1:
        return binary_search(ys, m+1, u)
    else:
        return binary_search(xs, l, m)

def test():
    lst = [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6]
    assert(min_free(lst), brute_force(lst))
    assert(dc_min_free(lst), brute_force(lst))

if __name__ == "__main__":
    test()

#!/usr/bin/python

import random

# method 1, d & c

# [l, u)
def solve(xs):
    l = 0
    u = len(xs)
    while l<u:
        m = (l+u)/2
        left = [x for x in xs if x < m]
        right = [x for x in xs if x >= m]
        if len(left) < m - l:
            lost = (m - 1 + l)*(m - l)/2 - sum(left)
            dup  = sum(right) - (u - 1 + m)*(u - m)/2
            return (lost, dup)
        elif len(left) > m - l:
            lost = (u - 1 + m)*(u - m)/2 - sum(right)
            dup  = sum(left) - (m - 1 + l)*(m - l)/2
            return (lost, dup)
        else:
            if sum(left) == (m -1 + l)*(m - l)/2:
                l = m
                xs = right
            else:
                u = m
                xs = left

# method 1', in-place D & C

def partition(xs, l, u, x):
    left = l
    for right in range(l, u):
        if xs[right] < x:
            (xs[left], xs[right]) = (xs[right], xs[left])
            left = left + 1
    return left

def solve_inplace(xs):
    (l, u) = (0, len(xs))
    while l<u:
        m = (l+u)/2
        m1 = partition(xs, l, u, m)
        (nl, nr) = (m1 - l, u - m1);
        (sl, sr) = (sum(xs[l:m1]), sum(xs[m1:u]))
        sl1 = (l + m - 1)*(m - l)/2
        sr1 = (m + u - 1)*(u - m)/2
        if m1 < m:
            return (sl1 - sl, sr - sr1)
        elif m1 > m:
            return (sr1 - sr, sl - sl1)
        else:
            if sl == sl1:
                l = m1
            else:
                u = m1

# method 2,
#   a = dup-lost
#   b = dup^2 - lost^2 = (dup+lost)*(dup-lost))
def solve2(xs):
    ys = zip(xs, range(len(xs)))
    a = sum(map(lambda (a, b): a-b, ys))
    b = sum(map(lambda (a, b): a*a - b*b, ys))
    return ((b/a - a)/2, (a + b/a)/2)

# method 3, contains bug
def solve3(xs):
    for i in range(len(xs)):
        while xs[i] != i:
            j = xs[i]
            if xs[j] == xs[i]:
                return (i, xs[i])
            else:
                j = xs[i]
                (xs[i], xs[j]) = (xs[j], xs[i])

def test_solve():
    for i in range(100):
        n = random.randint(0, 10000)
        xs = range(n)
        lost = random.choice(xs)
        xs.remove(lost)
        dup = random.choice(xs)
        xs.append(dup)
        random.shuffle(xs)
        assert(solve(xs[0:n]) == (lost, dup))
        assert(solve_inplace(xs[0:n]) == (lost, dup))
        __assert(solve3(xs[0:n]), (lost, dup))

def __assert(x, y):
    if x != y:
        print x, y
        assert(x == y)

def test():
    test_solve()
    
if __name__=="__main__":
    test()

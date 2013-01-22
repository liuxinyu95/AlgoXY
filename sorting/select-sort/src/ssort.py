#!/usr/bin/python

import random

def naive_ssort(xs):
    ys = []
    while xs != []:
        m = min(xs)
        xs.remove(m)
        ys.append(m)
    return ys

def in_place_ssort(xs):
    n = len(xs)
    for i in range(n):
        m = min_at(xs, i, n)
        (xs[i], xs[m]) = (xs[m], xs[i])
    return xs

def min_at(xs, i, n):
    m = i;
    for j in range(i+1, n):
        if xs[j] < xs[m]:
            m = j
    return m

def cocktail_sort(xs):
    n = len(xs)
    for i in range(n / 2):
        (mi, ma) = (i, n - 1 -i)
        if xs[ma] < xs[mi]:
            (xs[mi], xs[ma]) = (xs[ma], xs[mi])
        for j in range(i+1, n - 1 - i):
            if xs[j] < xs[mi]:
                mi = j
            if xs[ma] < xs[j]:
                ma = j
        (xs[i], xs[mi]) = (xs[mi], xs[i])
        (xs[n - 1 - i], xs[ma]) = (xs[ma], xs[n - 1 - i])
    return xs

def cocktail_sort1(xs):
    n = len(xs)
    for i in range(n / 2):
        (mi, ma) = (i, i)
        for j in range(i+1, n - i):
            if xs[j] < xs[mi]:
                mi = j
            if xs[ma] < xs[j]:
                ma = j
        if (ma < mi):
            (xs[mi], xs[ma]) = (xs[ma], xs[mi])
            (mi, ma) = (ma, mi)
        (xs[i], xs[mi]) = (xs[mi], xs[i])
        (xs[n - 1 - i], xs[ma]) = (xs[ma], xs[n - 1 - i])
    return xs


def test_sort(f, xs):
    #assert sorted(xs) == f([y for y in xs])
    ys = [y for y in xs]
    if sorted(xs) != f(ys):
        print "xs        =", xs
        print "ys        =", ys
        print "sorted(xs)=", sorted(xs)
        exit()

def test():
    for _ in xrange(100):
        n = random.randint(0, 100)
        xs = random.sample(range(100), n)
        test_sort(naive_ssort, xs)
        test_sort(in_place_ssort, xs)
        test_sort(cocktail_sort, xs)
        test_sort(cocktail_sort1, xs)

if __name__ == "__main__":
    #print cocktail_sort1([55, 52, 53, 99, 78, 16, 2, 41, 9, 39, 28, 37, 24, 19, 47, 83, 66, 92])
    test()


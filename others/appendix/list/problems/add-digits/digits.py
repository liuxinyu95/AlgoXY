#!/usr/bin/python

from random import * # for verification purpose

class Node:
    def __init__(self, digit, next= None):
        self.d = digit
        self.next = next

def from_list(xs):
    return reduce(lambda h, x: Node(x, h), reversed(xs), None)

def to_list(h):
    xs = []
    while h is not None:
        xs.append(h.d)
        h = h.next
    return xs

def print_list(xs):
    print "->".join(to_list(xs))

# solution 1: in-place add, the result is stored in xs
def add(xs, ys):
    c = 0
    zs = xs
    prev = None
    while xs is not None and ys is not None:
        d = (xs.d + ys.d + c) % 10
        c = (xs.d + ys.d + c) // 10
        xs.d = d
        prev = xs
        xs = xs.next
        ys = ys.next
    if ys is not None:
        xs = ys
    while xs is not None and c != 0:
        d = (xs.d + c) % 10
        c = (xs.d + c) // 10
        prev.next = xs
        prev = xs
        xs.d = d
        xs = xs.next
    prev.next = Node(c) if c != 0 else xs
    return zs

# solution 2: create a new list of result.
def add1(xs, ys):
    c = 0
    zs = p = Node(-1) #sentinel
    while xs is not None and ys is not None:
        d = xs.d + ys.d + c
        c = 1 if d > 9 else 0
        p.next = Node(d % 10)
        (p, xs, ys) = (p.next, xs.next, ys.next)
    xs = xs if ys is None else ys
    while xs is not None:
        d = xs.d + c
        c = 1 if d > 9 else 0
        p.next = Node(d % 10)
        (p, xs) = (p.next, xs.next)
    if c != 0:
        p.next = Node(c)
    return zs.next

def from_int(n):
    return None if n == 0 else Node(n % 10, from_int(n // 10))

def exmaple():
    print_list(add(from_list([]), from_list([])))
    print_list(add(from_list([3, 1, 5]), from_list([5, 9, 2])))
    print_list(add(from_list([2]), from_list([9, 9, 9])))

def assert_eq(msg, expect, actual, x, y):
    a = to_list(expect)
    b = to_list(actual)
    if a != b:
        print msg, "Expect: ", a, "actual: ", b, "xs=", x, "ys=", y
        exit()

def test():
    N = 100000
    for _ in xrange(100):
        x = randint(0, N)
        y = randint(0, N)
        assert_eq("add", from_int(x+y), add(from_int(x), from_int(y)), x, y)
        assert_eq("add1", from_int(x+y), add1(from_int(x), from_int(y)), x, y)
    print "Passed 100 cases."

if __name__ == "__main__":
    test()

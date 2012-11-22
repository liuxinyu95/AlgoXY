#!/usr/bin/python

# Imperative list manipulations

class List:
    def __init__(self, x = None, xs = None):
        self.key = x
        self.next = xs

def cons(x, xs):
    return List(x, xs)

def mapL(f, xs):
    ys = prev = List()
    while xs is not None:
        prev.next = List(f(xs.key))
        prev = prev.next
        xs = xs.next
    return ys.next

# auxiliar functions
def fromList(xs):
    ys = None
    for x in reversed(xs):
        ys = cons(x, ys)
    return ys

def toList(xs):
    ys = []
    while xs is not None:
        ys.append(xs.key)
        xs = xs.next
    return ys

# testing
def test_map():
    assert toList(mapL(lambda x: -x, fromList(range(10)))) == map(lambda x: -x, range(10))

if __name__ == "__main__":
    test_map()

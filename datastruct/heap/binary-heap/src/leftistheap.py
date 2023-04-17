from random import sample, randint

class Node:
    def __init__(self, value, rank = 1, left = None, right = None, parent = None):
        self.value = value
        self.rank = rank
        self.left = left
        self.right = right
        self.parent = parent

def rank(x):
    return x.rank if x else 0

def merge(a, b):
    h = Node(None)  # the sentinel node
    while a and b:
        if b.value < a.value:
            a, b = b, a
        c = Node(a.value, parent = h, left = a.left)
        h.right = c
        h = c
        a = a.right
    h.right = a if a else b
    while h.parent:
        if rank(h.left) < rank(h.right):
            h.left, h.right = h.right, h.left
        h.rank = 1 + rank(h.right)
        h = h.parent
    h = h.right
    if h:
        h.parent = None
    return h

def insert(h, x):
    return merge(Node(x), h)

def top(h):
    return h.value

def pop(h):
    return h.value, merge(h.left, h.right)

def fromlist(xs):
    h = None
    for x in xs:
        h = insert(h, x)
    return h

def hsort(xs):
    h = fromlist(xs)
    ys = []
    while h:
        y, h = pop(h)
        ys.append(y)
    return ys

def test(f):
    for _ in range(100):
        xs = sample(range(100), randint(0, 100))
        f(xs)
    print(f"100 tests for {f} passed.")

def prop_heap(xs):
    h = fromlist(xs)
    if xs:
        a = top(h)
        b = min(xs)
        assert a == b, f"violate heap: top = {a}, min = {b}"

def prop_heapsort(xs):
    ys = hsort(xs)
    zs = sorted(xs)
    assert ys == sorted(xs), f"violate heap sort, xs = {xs}, ys = {ys}, zs = {zs}"

if __name__ == "__main__":
    test(prop_heap)
    test(prop_heapsort)

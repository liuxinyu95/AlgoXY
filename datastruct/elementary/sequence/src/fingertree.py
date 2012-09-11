#!usr/bin/python

import random

# definition
class Node:
    def __init__(self, s, xs):
        self.size = s
        self.children = xs

    def str(self):
        if self.size == 1:
            return ", ".join([str(x) for x in self.children])
        else:
            return "(" + ", ".join([x.str() for x in self.children]) + ")"

class Tree:
    def __init__(self, s, f, m, r):
        self.size = s
        self.front = f
        self.rear = r
        self.mid = m

    def empty(self):
        return self.size == 0

    def str(self):
        if self.empty():
            return "."
        else:
            return "{" + ns2str(self.front) + " " +  tr2str(self.mid) + " "+ ns2str(self.rear) + "}"

# Auxiliary functions for debugging

def ns2str(ns):
    return "[" + ", ".join([n.str() for n in ns]) + "]"

def tr2str(t):
    return "." if t is None else t.str()

# Helper functions
def sizeNs(xs):
    return sum(map(lambda x: x.size, xs))

def sizeT(t):
    if t is None:
        return 0 
    else: 
        return t.size

def wrap(x):
    return Node(1, [x])

def wraps(xs):
    return Node(sizeNs(xs), xs)

def nodes(xs):
    res = []
    while len(xs) > 4:
        res.append(wraps(xs[:3]))
        xs = xs[3:]
    if len(xs) == 4:
        res.append(wraps(xs[:2]))
        res.append(wraps(xs[2:]))
    elif xs != []:
        res.append(wraps(xs))
    return res

def frontFull(t):
    return t is not None and len(t.front) >= 3

def rearFull(t):
    return t is not None and len(t.rear) >= 3

def isLeaf(t):
    return t is not None and len(t.front) == 1 and t.rear ==[]

def isBranch(t):
    return t is not None and (not isLeaf(t))

def normalize(t):
    if t is not None and t.front == [] and len(t.rear) == 1:
        (t.front, t.rear) = (t.rear, t.front)
    return t

def fromNs(ns):
    #fold-right(cons, ns, None)
    t = None
    for n in reverse(ns):
        t = cons(n, t)
    return t

def tree(f, m, r):
    while f == [] or r == []:
        if m is None:
            if r == []:
                return fromNs(f)
            if f == []:
                return fromNs(r)
        else:
            if r == []:
                (m, r1) = unsnoc(m)
                r = r1.children
            elif f == []:
                (f1, m) = uncons(m)
                f=f1.children
    return Tree(sizeNs(f) + sizeT(m) + sizeNs(r), f, m, r)

def insert(x, t):
    return cons(wrap(x), t)

# inserting a node in single-pass manner
def cons(n, t):
    root = t
    prev = None
    while frontFull(t):
        f = t.front
        t.front = [n] + f[:1]
        t.size = t.size + n.size
        n = wraps(f[1:])
        prev = t
        t = t.mid
    if t is None:
        t = Tree(n.size, [n], None, [])
    elif isLeaf(t):
        t = Tree(n.size + t.size, [n], None, t.front)
    else:
        t = Tree(n.size + t.size, [n]+t.front, t.mid, t.rear)
    if prev is not None:
        prev.mid = t
    else:
        root = t
    return root

# extract the first node from tree
# assume t is not None
def uncons(t):
    root = t
    prev = None
    x = head(t)
    # a repeat - until loop
    while True:
        t.size = t.size - t.front[0].size
        t.front = t.front[1:]
        if t.mid is not None and t.front == []:
            prev = t
            t = t.mid
            prev.front = t.front[0].children
        else:
            break
    if t.mid is None and t.front == []:
        if t.rear == []:
            t = None
        elif len(t.rear) == 1:
            t = Tree(t.size, t.rear, None, [])
        else:
            t = Tree(t.size, t.rear[:1], None, t.rear[1:])
    if prev is not None:
        prev.mid = t
    else:
        root = t
    return (x, root)

def head(t):
    return t.front[0].children[0]

def tail(t):
    (_, t) = uncons(t)
    return t

def append(t, x):
    return snoc(t, wrap(x))

def snoc(t, n):
    root = t
    prev = None
    while rearFull(t):
        r = t.rear
        t.rear = r[-1:] + [n]
        t.size = t.size + n.size
        n = wraps(r[:-1])
        prev = t
        t = t.mid
    if t is None:
        t = Tree(n.size, [n], None, [])
    elif len(t.rear) == 1 and t.front == []:
        t = Tree(n.size + t.size, t.rear, None, [n])
    else:
        t = Tree(n.size + t.size, t.front, t.mid, t.rear + [n])
    if prev is not None:
        prev.mid = t
    else:
        root = t
    return root

def unsnoc(t):
    root = t
    prev = None
    x = last(t)
    if t.size == 1:
        return (x, None)
    while True:
        t.size = t.size - t.rear[-1].size
        t.rear = t.rear[:-1]
        if t.mid is not None and t.rear == []:
            prev = t
            t = t.mid
            if isLeaf(t):
                t = Tree(t.size, [], None, t.front)
            prev.rear = t.rear[-1].children
        else:
            break
    if t.mid is None and t.rear == []:
        if t.front == []:
            t = None
        else:
            t = Tree(t.size, t.front[:-1], None, t.front[-1:])
    if prev is not None:
        prev.mid = t
    else:
        root = t
    return (x, normalize(root))

def last(t):
    if t.size == 1:
        return head(t)
    return t.rear[-1].children[-1]

def init(t):
    (_, t) = unsnoc(t)
    return t

def concat(t1, t2):
    return merge(t1, [], t2)

def merge(t1, ns, t2):
    root = None
    prev = Tree(0, [], None, []) #sentinel dummy tree
    while isBranch(t1) and isBranch(t2):
        t = Tree(t1.size + t2.size + sizeNs(ns), t1.front, None, t2.rear)
        if root is None:
            root = t
        prev.mid = t
        prev = t
        ns = nodes(t1.rear + ns + t2.front)
        t1 = t1.mid
        t2 = t2.mid
    if isLeaf(t1):
        ns = t1.front + ns
        t1 = None
    if isLeaf(t2):
        ns = ns + t2.front
        t2 = None
    t = None
    if t1 is None:
        t = t2
        for n in reversed(ns):
            t = cons(n, t)
    elif t2 is None:
        t = t1
        for n in ns:
            t = snoc(t, n)
    prev.mid = t
    if root is None:
        root = t
    return root

# Auxiliary functions for verification

def fromList(xs):
    # fold-right insert None xs
    t = None
    for x in reversed(xs):
        t = insert(x, t)
    return t

def toList(t):
    xs = []
    while t is not None:
        (x, t) = uncons(t)
        xs.append(x)
    return xs

def fromList1(xs):
    return reduce(append, xs, None)

def toList1(t):
    xs = []
    while t is not None:
        (x, t) = unsnoc(t)
        xs.insert(0, x)
    return xs

def __assert(xs, ys):
    if xs != ys:
        print xs
        print ys
        quit()

def test_rebuild():
    for i in range(100):
        xs = range(i)
        assert toList(fromList(xs)) == xs
        assert toList(fromList1(xs)) == xs
        assert toList1(fromList(xs)) ==xs
        assert toList1(fromList1(xs)) == xs

def test_concat():
    m = 100
    for i in range(m):
        xs = random.sample(range(m), random.randint(0, m))
        ys = random.sample(range(m), random.randint(0, m))
        assert toList(concat(fromList(xs), fromList(ys))) == (xs + ys)

if __name__ == "__main__":
    test_rebuild()
    test_concat()

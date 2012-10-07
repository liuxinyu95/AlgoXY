#!usr/bin/python

import random

# As there is no fold-right in python
def foldR(f, xs, z):
    for x in reversed(xs):
        z = f(x, z)
    return z

# definition
class Node:
    def __init__(self, s, xs, lf = False):
        self.size = s
        self.children = xs
        self.leaf = lf

    def str(self):
        if self.leaf:
            return ", ".join([str(x) for x in self.children])
        else:
            return "(" + ", ".join([x.str() for x in self.children]) + ")"

class Tree:
    def __init__(self, s = 0, f = [], m = None, r = [], p = None):
        self.size = s
        self.front = f
        self.mid = m
        self.rear = r
        self.parent = p
        if m is not None:
            m.parent = self

    def set_mid(self, t):
        self.mid = t
        if t is not None:
            t.parent = self

    def empty(self):
        return self.size == 0

    # for debugging purpose only
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

# calculate the size for a list of nodes
def sizeNs(xs):
    return sum(map(lambda x: x.size, xs))

# calculate the size for a tree
def sizeT(t):
    if t is None:
        return 0 
    else: 
        return t.size

# wrap an element to a node
def wrap(x):
    return Node(1, [x], True)

# return the singleton element in a node
def elem(n):
    assert n.leaf
    return n.children[0]

# wraps a list of nodes (<= 3) to a big node
def wraps(xs):
    return Node(sizeNs(xs), xs)

# create a leaf contains only one node
def leaf(x):
    return Tree(x.size, [x], None, [])

# wrap a list of nodes (>3) to a list of big nodes
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

# test if the front finger is full, which can't afford any more nodes
def frontFull(t):
    return t is not None and len(t.front) >= 3

# test if the rear finger is full, which can't afford any more modes
def rearFull(t):
    return t is not None and len(t.rear) >= 3

def isBranch(t):
    return t is not None and t.front !=  [] and t.rear != []

# remove unused senitels if there are
def flat(t):
    while t is not None and t.empty():
        t = t.mid
    if t is not None:
        t.parent = None
    return t

def insert(x, t):
    return prepend_node(wrap(x), t)

# inserting a node in single-pass manner
def prepend_node(n, t):
    root = prev = Tree()
    prev.set_mid(t)
    while frontFull(t):
        f = t.front
        t.front = [n] + f[:1]
        t.size = t.size + n.size
        n = wraps(f[1:])
        prev = t
        t = t.mid
    if t is None:
        t = leaf(n)
    elif len(t.front)==1 and t.rear == []:
        t = Tree(n.size + t.size, [n], None, t.front)
    else:
        t = Tree(n.size + t.size, [n]+t.front, t.mid, t.rear)
    prev.set_mid(t)
    return flat(root)

# extract the first element from tree
# assume t is not empty
# capable to handle illed form (due to deletion or splitting)
def extract_head(t):
    root = Tree()
    root.set_mid(t)
    while t.front == [] and t.mid is not None:
        t = t.mid
    if t.front == [] and t.rear != []:
        (t.front, t.rear) = (t.rear, t.front)
    n = wraps(t.front)
    while True: # a repeat-until loop
        ns = n.children
        n = ns[0]
        t.front = ns[1:]
        t.size = t.size - n.size
        t = t.parent
        if t.mid.empty():
            t.mid.parent = None
            t.mid = None
        if n.leaf:
            break
    return (elem(n), flat(root))

# return the first element without removing it
def first(t):
    return elem(first_leaf(t))

def first_leaf(t):
    while t.front == [] and t.mid is not None:
        t = t.mid
    if t.front == [] and t.rear != []:
        n = t.rear[0]
    else:
        n = t.front[0]
    while not n.leaf:
        n = n.children[0]
    return n

# Note this will mutate t.
def tail(t):
    (_, t) = extract_head(t)
    return t

def append(t, x):
    return append_node(t, wrap(x))

def append_node(t, n):
    root = prev = Tree()
    prev.set_mid(t)
    while rearFull(t):
        r = t.rear
        t.rear = r[-1:] + [n]
        t.size = t.size + n.size
        n = wraps(r[:-1])
        prev = t
        t = t.mid
    if t is None:
        t = leaf(n)
    elif len(t.rear) == 1 and t.front == []:
        t = Tree(n.size + t.size, t.rear, None, [n])
    else:
        t = Tree(n.size + t.size, t.front, t.mid, t.rear + [n])
    prev.set_mid(t)
    return flat(root)

# extract the last element from tree
# assume t is not empty
# capable to handle illed-form (due to deletion or splitting)
def extract_tail(t):
    root = Tree()
    root.set_mid(t)
    while t.rear == [] and t.mid is not None:
        t = t.mid
    if t.rear == [] and t.front != []:
        (t.front, t.rear) = (t.rear, t.front)
    n = wraps(t.rear)
    while True: # a repeat-until loop
        ns = n.children
        n = ns[-1]
        t.rear = ns[:-1]
        t.size = t.size - n.size
        t = t.parent
        if t.mid.empty():
            t.mid.parent = None
            t.mid = None
        if n.leaf:
            break
    return (elem(n), flat(root))

# return the last element without removing it.
def last(t):
    while t.rear == [] and t.mid is not None:
        t = t.mid
    if t.rear ==[] and t.front != []:
        n = t.front[-1]
    else:
        n = t.rear[-1]
    while not n.leaf:
        n = n.children[-1]
    return elem(n)

# Note this will mutate the tree
def init(t):
    (_, t) = extract_tail(t)
    return t

# Note this will mutate t1 and t2
def concat(t1, t2):
    return merge(t1, [], t2)

def merge(t1, ns, t2):
    root = prev = Tree() #sentinel dummy tree
    while t1 is not None and t2 is not None:
        t = Tree(t1.size + t2.size + sizeNs(ns), t1.front, None, t2.rear)
        prev.set_mid(t)
        prev = t
        ns = nodes(t1.rear + ns + t2.front)
        t1 = t1.mid
        t2 = t2.mid
    if t1 is None:
        prev.set_mid(foldR(prepend_node, ns, t2))
    elif t2 is None:
        prev.set_mid(reduce(append_node, ns, t1))
    return flat(root)

def getAt(t, i):
    return applyAt(t, i, lambda x : x) # applying id function

def setAt(t, i, x):
    return applyAt(t, i, lambda y : x)

# apply function f to the element at i position
# return the original element before applying f
def applyAt(t, i, f):
    while t.size > 1:
        szf = sizeNs(t.front)
        szm = sizeT(t.mid)
        if i < szf:
            return lookupNs(t.front, i, f)
        elif i < szf + szm:
            t = t.mid
            i = i - szf
        else:
            return lookupNs(t.rear, i - szf - szm, f)
    n = first_leaf(t)
    x = elem(n)
    n.children[0] = f(x)
    return x

# lookup in a list of node for position i, and 
# apply function f to the element
# return the original element before applying f
def lookupNs(ns, i, f):
    while True:
        for n in ns:
            if n.leaf and i == 0:
                x = elem(n)
                n.children[0] = f(x)
                return x
            if i < n.size:
                ns = n.children
                break
            i = i - n.size

def szf(t):
    return sizeNs(t.front)

def szm(t):
    return sizeT(t.mid)

# split(t, i) ==> (t1, x, t2)
def splitAt(t, i):
    # top-down pass
    (t1, t2) = (Tree(), Tree())
    while szf(t) <= i and i < szf(t) + szm(t):
        fst = Tree(0, t.front, None, [])
        snd = Tree(0, [], None, t.rear)
        t1.set_mid(fst)
        t2.set_mid(snd)
        (t1, t2) = (fst, snd)
        i = i - szf(t)
        t = t.mid

    if i < szf(t):
        (xs, n, ys) = splitNs(t.front, i)
        sz = t.size - sizeNs(xs) - n.size
        (fst, snd) = (fromNodes(xs), Tree(sz, ys, t.mid, t.rear))
    elif szf(t) + szm(t) <= i:
        (xs, n, ys) = splitNs(t.rear, i - szf(t) - szm(t))
        sz = t.size - sizeNs(ys) - n.size
        (fst, snd) = (Tree(sz, t.front, t.mid, xs), fromNodes(ys))
    t1.set_mid(fst)
    t2.set_mid(snd)

    # bottom-up pass
    i = i - sizeT(fst)
    while not n.leaf:
        (xs, n, ys) = splitNs(n.children, i)
        i = i - sizeNs(xs)
        (t1.rear, t2.front) = (xs, ys)
        t1.size = sizeNs(t1.front) + sizeT(t1.mid) + sizeNs(t1.rear)
        t2.size = sizeNs(t2.front) + sizeT(t2.mid) + sizeNs(t2.rear)
        (t1, t2) = (t1.parent, t2.parent)

    return (flat(t1), elem(n), flat(t2))

def splitNs(ns, i):
    for j in range(len(ns)):
        if i < ns[j].size:
            return (ns[:j], ns[j], ns[j+1:])
        i = i - ns[j].size

def removeAt(t, i):
    (t1, x, t2) = splitAt(t, i)
    return (x, concat(t1, t2))

# Auxiliary functions for verification

def fromListR(xs):
    return foldR(insert, xs, None)

def toListR(t):
    xs = []
    while t is not None:
        (x, t) = extract_head(t)
        xs.append(x)
    return xs

def fromListL(xs):
    return reduce(append, xs, None)

def toListL(t):
    xs = []
    while t is not None:
        (x, t) = extract_tail(t)
        xs.insert(0, x)
    return xs

def fromNodes(ns):
    return foldR(prepend_node, ns, None)

def __assert(xs, ys):
    if xs != ys:
        print "assertion failed!"
        print xs
        print ys
        quit()

def test_rebuild():
    for i in range(100):
        xs = range(i)
        assert toListR(fromListR(xs)) == xs
        assert toListR(fromListL(xs)) == xs
        assert toListL(fromListR(xs)) ==xs
        assert toListL(fromListL(xs)) == xs
    print "rebuild tested"

def test_concat():
    m = 100
    for i in range(m):
        xs = random.sample(range(m), random.randint(0, m))
        ys = random.sample(range(m), random.randint(0, m))
        assert toListR(concat(fromListR(xs), fromListR(ys))) == (xs + ys)
    print "concat tested"

def test_random_access():
    xs = range(100)
    t = fromListR(xs)
    ys = [getAt(t, i) for i in xs]
    assert xs == ys
    [setAt(t, i, 99 - xs[i]) for i in xs]
    ys = [getAt(t, i) for i in xs]
    xs.reverse()
    __assert(xs, ys)
    print "random access tested"

def test_split():
    for i in range(100):
        lst = range(100)
        (xs, y, ys) = (lst[:i], lst[i], lst[i+1:])
        t = fromListR(lst)
        (t1, x, t2) = splitAt(t, i)
        __assert(xs, toListR(t1))
        __assert(ys, toListR(t2))
        assert(x == y)
    print "split tested"

def test_remove():
    m = 100
    for _ in range(100):
        xs = random.sample(range(m), random.randint(1, m))
        i = random.randint(0, len(xs)-1)
        (y, t) = removeAt(fromListR(xs), i)
        (x, ys) = (xs[i], xs[:i] + xs[i+1:])
        __assert(ys, toListR(t))
        assert y == x

if __name__ == "__main__":
    test_rebuild()
    test_concat()
    test_random_access()
    test_split()
    test_remove()

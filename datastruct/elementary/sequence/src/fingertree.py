#!usr/bin/python

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

def ns2str(ns):
    return "[" + ", ".join([n.str() for n in ns]) + "]"

def tr2str(t):
    return "." if t is None else t.str()

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

def fromNs(ns):
    #fold-right(cons, ns, None)
    t = None
    for n in reverse(ns):
        t = cons(n, t)
    return t

def isFull(t):
    return t is not None and len(t.front) >= 3

def isLeaf(t):
    return t is not None and len(t.front) == 1 and t.rear ==[]

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
    while isFull(t):
        f = t.front
        t.front = [n] + f[:1]
        t.size = t.size + n.size
        n = wraps(f[1:])
        prev = t
        t = t.mid
    if t is None:
        t = Tree(n.size, [n], None, [])
    elif isLeaf(t):
        t = tree([n], None, t.front)
    else:
        t = tree([n]+t.front, t.mid, t.rear)
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
    x = t.front[0]
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
            t = Tree(t.size, t.rear[:1], None, t.rear[:1])
    if prev is not None:
        prev.mid = t
    else:
        root = t
    return (x.children[0], root)

def head(t):
    return t.front[0].children[0]

def tail(t):
    (_, t) = uncons(t)
    return t

# Auxiliary functions

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

def test_head():
    for i in range(100):
        xs = range(i)
        assert toList(fromList(xs)) == xs

if __name__ == "__main__":
    test_head()

#!/usr/bin/python

import random

# method 1, array based, each cell record both element and the min so far.
def push1(s, x):
    if s:
        (_, m) = s[-1]
        s.append((x, min(m, x)))
    else:
        s.append((x, x))
    return s

def pop1(s):
    (x, _) = s.pop()
    return x, s

def min1(s):
    (_, m) = s[-1]
    return m

def top1(s):
    (x, _) = s[-1]
    return x

def is_empty1(s):
    return not s

# method 2, linked-list based, each node record a pointer to the min
class Node:
    def __init__(self, x, next=None):
        self.key = x
        self.next = next
        self.min = None

def push2(s, x):
    h = Node(x, s)
    h.min = h if s is None or x < s.min.key else s.min
    return h

def pop2(s):
    h = s.next
    s.next = None
    return s.key, h

def min2(s):
    return s.min.key

def top2(s):
    return s.key

def is_empty2(s):
    return not s

# method 3, double stacks. One stack for elements, another stack for the minimum elements.
def empty():
    return ([], [])

def push3(s, x):
    (s1, s2) = s
    s1.append(x)
    if not s2 or x <= s2[-1]:
        s2.append(x)
    return (s1, s2)

def pop3(s):
    (s1, s2) = s
    x = s1.pop()
    if s2 and s2[-1] == x:
        s2.pop()
    return x, (s1, s2)

def min3(s):
    (_, s2) = s
    return s2[-1]

def top3(s):
    (s1, _) = s
    return s1[-1]

def is_empty3(s):
    return s == ([], [])

def test():
    s1 = []
    s2 = None
    s3 = empty()
    for _ in xrange(100):
        if random.randint(0, 1) == 0:
            x = random.randint(1, 100)
            s1 = push1(s1, x)
            s2 = push2(s2, x)
            s3 = push3(s3, x)
            assert(top1(s1) == top2(s2) and top1(s1) == top3(s3))
            assert(min1(s1) == min2(s2) and min1(s1) == min3(s3))
        else:
            if is_empty1(s1):
                assert(is_empty2(s2) and is_empty3(s3))
            else:
                assert(min1(s1) == min2(s2) and min1(s1) == min3(s3))
                x1, s1 = pop1(s1)
                x2, s2 = pop2(s2)
                x3, s3 = pop3(s3)
                assert(x1 == x2 and x1 == x3)
    print "100 cases passed"

if __name__ == "__main__":
    test()

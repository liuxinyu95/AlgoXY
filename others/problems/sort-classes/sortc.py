#!/usr/bin/python

# sortc.py
# Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# TODO: License...

import string

class Item:
    def __init__(self):
        self.key = ""
        self.pre = []

def create_item(s):
    (c, pres) = s.split(':')
    x = Item()
    x.key = c
    x.pre = pres.split()
    return x

def schedule(ss):
    items = []
    for s in ss:
        items.append(create_item(s))
    res = []
    while items != []:
        try:
            c = items.pop(0)
            insert(res, c, items)
        except LookupError:
            (res, items) = ([], [])
    return map(lambda x: x.key, res)

# returns the position where c is inserted.
def insert(res, c, cs):
    from_pos = -1
    for x in c.pre:
        pos = lookup(res, x)
        if pos is None:
            (c1, cs) = extract(cs, x)
            pos = insert(res, c1, cs)
        from_pos = max(from_pos, pos)
    return insert_from(res, from_pos, c)
        
def lookup(lst, x):
    for i in range(len(lst)):
        if lst[i].key == x:
            return i
    return None

def extract(lst, x):
    pos = lookup(lst, x)
    if pos is None:
        raise LookupError
    else:
        i = lst.pop(pos)
        return (i, lst)

def insert_from(lst, pos, x):
    i = pos + 1
    while i < len(lst):
        if less_than(x, lst[i]):
            break
        i = i+1
    lst.insert(i, x)
    return i

def less_than(x, y):
    (s1, n1) = parse_class(x)
    (s2, n2) = parse_class(y)
    return (n1 < n2) or (n1 == n2 and s1 < s2)

def parse_class(s):
    name = ""
    num  = 0
    for c in s.key:
        if c in string.digits:
            num = num * 10 + int(c)
        else:
            name = name + c
    return (name, num)

def test():
    cs1 = ["CSE121: CSE110", "CSE110:", "MATH122:"]
    r1 = ["CSE110","CSE121","MATH122"]
    cs2 = ["ENGL111: ENGL110", "ENGL110: ENGL111"]
    r2 = []
    cs3 = ["ENGL111: ENGL110"]
    r3 = []
    cs4 = ["CSE258: CSE244 CSE243 INTR100", 
           "CSE221: CSE254 INTR100",
           "CSE254: CSE111 MATH210 INTR100",
           "CSE244: CSE243 MATH210 INTR100",
           "MATH210: INTR100",
           "CSE101: INTR100",
           "CSE111: INTR100",
           "ECE201: CSE111 INTR100",
           "ECE111: INTR100",
           "CSE243: CSE254",
           "INTR100:"]
    r4 = ["INTR100","CSE101","CSE111","ECE111",
          "ECE201","MATH210","CSE254","CSE221","CSE243",
          "CSE244","CSE258"]
    __assert(schedule(cs1), r1)
    __assert(schedule(cs2), r2)
    __assert(schedule(cs3), r3)
    __assert(schedule(cs4), r4)

def __assert(x, y):
    if x == y:
        print "OK"
    else:
        print "Fail:", x, "!=", y

if __name__ == "__main__":
    test()


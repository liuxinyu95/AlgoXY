#!/usr/bin/python

# patn.py
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


# {a#b}c{d#e{f#g}} 

# parse the rule str to list
#  ==> [ [a,b], c, [d, [e, [f, g]]] ]

# BNF syntax:
#
# rule = term + rule
# term = token | lst
# token = a..z
# lst = { rule # lst }

def parse(s):
    (r, _) = rule(filter(lambda x: x!=' ', s)) #remove all spaces before parsing
    return r

def rule(s):
    xs = []
    while s != "" and (s[0] not in '#}'):
        (x, s) = term(s)
        xs.append(x)
    return (xs, s)

def term(s):
    if s[0] == '{':
        return lst(s)
    else:
        return token(s)

# token = a..z
def token(s):
    x = ''
    while s != "" and (s[0] not in "#{}"):
        x = x + s[0]
        s = s[1:]
    return (x, s)

# lst = { rule # lst }
def lst(s):
    xs = []
    s = s[1:] # consume '{'
    while s!="" and s[0]!='}':
        if s[0] == '#':
            s = s[1:]
        (r, s) = rule(s)
        xs.append(r)
    return (xs, s[1:]) # consume '}'

def gen(xs):
    return reduce(merge, map(gen_term, xs))

def gen_term(xs):
    if xs.__class__ == str:
        return [xs]
    return reduce(lambda x, y: x+y, map(gen, xs))

def merge(xs, ys):
    if ys == []:
        return xs
    if xs == []:
        return ys
    return [x+y for x in xs for y in ys]

def test_parse():
    lst=parse("{a # b} c {d # e {f # g}}")
    print lst
    return lst

def test_gen():
    print gen(test_parse())

def test():
    test_gen()

if __name__ == "__main__":
    test()

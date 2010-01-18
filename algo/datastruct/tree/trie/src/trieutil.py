#!/usr/bin/python

import string

def trie_to_str(t, prefix=""):
    str="("+prefix
    if t.value is not None:
        str += ":"+t.value
    for k,v in sorted(t.children.items()):
        str += ", "+trie_to_str(v, prefix+k)
    str+=")"
    return str

def from_list(l, insert_func):
    t = None
    for x in l:
        t = insert_func(t, x)
    return t

def from_map(m, insert_func):
    t = None
    for k, v in m.items():
        t = insert_func(t, k, v)
    return t

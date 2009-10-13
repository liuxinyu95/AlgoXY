#!/usr/bin/python

import string

class Trie:
    def __init__(self):
        self.value = ""
        self.children = {}

def trie_insert(t, key):
    if t is None:
        t = Trie();

    p = t
    for c in key:
        if c.isdigit():
            c = eval(c)
        if not c in p.children: 
            p.children[c] = Trie()
        p = p.children[c]
    p.value = key # this is only for illustration purpose
    return t

def trie_to_str(t, prefix=""):
    to_str = lambda x: "%s" %x
    str="("+prefix
    for (k,v) in t.children.items():
        str += ", "+trie_to_str(v, prefix+to_str(k))
    str+=")"
    return str

def list_to_trie(l):
    t = None
    for x in l:
        t = trie_insert(t, x)
    return t

class TrieTest:
    def __init__(self):
        print "start trie test"

    def run(self):
        self.test_insert()

    def test_insert(self):
        t = None
        t = trie_insert(t, "a");
        t = trie_insert(t, "an");
        t = trie_insert(t, "another");
        t = trie_insert(t, "b");
        t = trie_insert(t, "bob");
        t = trie_insert(t, "bool");
        t = trie_insert(t, "home");
        print trie_to_str(t)
        t1 = list_to_trie(["011", "11", "0011"])
        print trie_to_str(t1)

def main(argv):
    (filename, type, ts)=get_args(argv)
    if type == "trie":
        t=str_to_trie(ts)
        trie_to_dot(t, filename)

if __name__ == "__main__":
    TrieTest().run()

#reference
#[1] http://www.graphviz.org/mywiki/FaqBalanceTree

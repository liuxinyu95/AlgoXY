#!/usr/bin/python

import string
import trie
import patricia

# expand all sub branches with a prefix string
def expand(prefix, t, n):
    res = []
    q = [(prefix, t)]
    while len(res)<n and len(q)>0:
        (s, p) = q.pop(0)
        if p.value is not None:
            res.append((s, p.value))
        for k, tr in p.children.items():
            q.append((s+k, tr))
    return res

# lookup top N candidate start from key
def trie_lookup(t, key, n):
    if t is None:
        return None

    p = t
    for c in key:
        if not c in p.children:
            return None
        p=p.children[c]
    return expand(key, p, n)

def patricia_lookup(t, key, n):
    if t is None:
        return None
    prefix = ""
    while(True):
        match = False
        for k, tr in t.children.items():
            if string.find(k, key) == 0: #is prefix of
                return expand(prefix+k, tr, n)
            if string.find(key, k) ==0:
                match = True
                key = key[len(k):]
                t = tr
                prefix += k
                break
        if not match:
            return None
                
def trie_lookup_t9(t, key):
    pass

def patricia_lookup_t9(t, key):
    pass

class LookupTest:
    def __init__(self):
        print "word auto-completion and T9 test"
        dict = {"a":"the first letter of English", \
           "an":"used instead of 'a' when the following word begins with a vowel sound", \
           "another":"one more person or thing or an extra amount", \
           "abandon":"to leave a place, thing or person forever",\
           "about":"on the subject of; connected with",\
           "adam":"a character in the Bible who was the first man made by God",\
           "boy":"a male child or, more generally, a male of any age", \
           "bodyl":"the whole physical structure that forms a person or animal", \
           "zoo":"an area in which animals, especially wild animals, are kept so that people can go and look at them, or study them"}
        self.tt = trie.map_to_trie(dict)
        self.tp = patricia.map_to_patricia(dict)

    def run(self):
        self.test_trie_lookup()
        self.test_patricia_lookup()
        self.test_trie_t9()
        self.test_patricia_t9()

    def test_trie_lookup(self):
        print "test lookup top 5 in Trie"
        print "search a ", trie_lookup(self.tt, "a", 5)
        print "search ab ", trie_lookup(self.tt, "ab", 5)

    def test_patricia_lookup(self):
        print "test lookup top 5 in Patricia"
        print "search a ", patricia_lookup(self.tp, "a", 5)
        print "search ab ", patricia_lookup(self.tp, "ab", 5)

    def test_trie_t9(self):
        pass

    def test_patricia_t9(self):
        pass

if __name__ == "__main__":
    LookupTest().run()

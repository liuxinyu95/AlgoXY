#!/usr/bin/python

import string
import trie
import patricia

# lookup top N candidate start from key
def trie_lookup_N(t, key, n):
    if t is None:
        return None

    p = t
    for c in key:
        if not c in p.children:
            return None
        p=p.children[c]
    # expand all
    res = []
    q = [(key, p)]
    while len(res)<n and len(q)>0:
        (s, p) = q.pop(0)
        if p.value is not None:
            res.append((s, p.value))
        for k, tr in p.children.items():
            q.append((s+k, tr))
    return res

def patricia_lookup_N(t, key):
    pass

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
        self.test_trie_lookup_N()
        self.test_patricia_lookup_N()
        self.test_trie_t9()
        self.test_patricia_t9()

    def test_trie_lookup_N(self):
        print "test lookup to 5"
        print "search a ", trie_lookup_N(self.tt, "a", 5)
        print "search ab ", trie_lookup_N(self.tt, "ab", 5)

    def test_patricia_lookup_N(self):
        pass

    def test_trie_t9(self):
        pass

    def test_patricia_t9(self):
        pass

if __name__ == "__main__":
    LookupTest().run()

#!/usr/bin/python

import random
import hashlib # for bloom filer

C = ["Beijing", "Shanghai", "Hong Kong", "Tianjin", "Wuhan", "Guangzhou",
     "Shenzhen", "Shenyang", "Chongqing", "Taipei", "Xian", "Chengdu",
     "Changchun", "Hangzhou", "Jinan", "Dalian", "Taiyuan", "Zhengzhou",
     "Qingdao", "Shijiazhuang", "Kunming", "Lanzhou", "Gaoxiong", "Zibo"]

WORD = 4

def init(lst):
    idx = {}
    for i in range(len(C)):
        idx[C[i]] = i
    return idx

#def bitmap(idx, lst):
#    bits = [0]*len(C)
#    for k in lst:
#        bits[idx[k]] = 1
#    return bits

# compressed bitmap
def bitmap(idx, lst):
    bits = [0] * (len(C)/WORD)
    for k in lst:
        setbit(bits, idx[k])
    return bits

def setbit(bits, i):
    bits[i/WORD] |= 1<<(i % WORD)

def testbit(bits, i):
    return bits[i/WORD] & (1<<(i % WORD)) != 0

#def elem(idx, bits, x):
#    return bits[idx[x]]==1

def elem(idx, bits, x):
    return testbit(bits, idx[x])

# method 2: using bloom filter

def init_hash():
    hashes = []
    hashes.append(hashlib.md5())
    hashes.append(hashlib.sha1())
    hashes.append(hashlib.sha224())
    return hashes

def apply_hash(hashes, x):
    r = 0
    for h in hashes:
        h.update(x)
        r |= int(h.hexdigest(), 16)
    return r

def create_filter(hashes, lst):
    bfilter = 0
    for k in lst: 
        bfilter |= apply_hash(hashes, k)
    return bfilter

def elem_bloom_filter(hashes, bfilter, x):
    r = apply_hash(hashes, x)
    return (r & bfilter) == r 

def test():
    idx = init(C)
    for i in range(100):
        k = random.randint(0, len(C))
        lst = random.sample(C, k)
        bits = bitmap(idx, lst)
        for i in range(100):
            x = random.choice(C)
            assert(elem(idx, bits, x) == (x in lst))
    print "OK"

def test_bloom_filter():
    hashes = init_hash()
    (failed, n) = (0, 0)
    for i in range(100):
        k = random.randint(0, len(C))
        lst = random.sample(C, k)
        bfilter = create_filter(hashes, lst)
        for i in range(100):
            x = random.choice(C)
            n = n + 1
            if elem_bloom_filter(hashes, bfilter, x) != (x in lst):
                failed = failed + 1
        if elem_bloom_filter(hashes, bfilter, "New York"):
            failed = failed + 1
        n = n + 1
    print "Bloom Filter failed rate", float(failed)/float(n)*100, "%"

if __name__ == "__main__":
    test()
    test_bloom_filter()

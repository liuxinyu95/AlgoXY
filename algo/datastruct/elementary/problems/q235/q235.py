#!/usr/bin/python

def get(k):
    q = [1]
    while k > 0:
        x = q.pop(0)
        q = uniq_insert(q, x*2)
        q = uniq_insert(q, x*3)
        q = uniq_insert(q, x*5)
        k = k - 1
    return x

def uniq_insert(q, x):
    i = 0
    while i < len(q) and q[i] < x:
        i = i+1
    if i < len(q) and x == q[i]:
        return q
    q.insert(i, x)
    return q
        
if __name__ == "__main__":
    print get(1500)

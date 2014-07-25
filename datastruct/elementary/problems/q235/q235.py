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
        
def get1(k):
    if k == 1:
        return 1
    (q2, q3, q5) = ([2], [3], [5])
    while k > 1:
        x = min(q2[0], q3[0], q5[0])
        if x == q2[0]:
            q2.pop(0)
            q2.append(x*2)
            q3.append(x*3)
            q5.append(x*5)
        elif x == q3[0]:
            q3.pop(0)
            q3.append(x*3)
            q5.append(x*5)
        else:
            q5.pop(0)
            q5.append(x*5)
        k = k - 1
    return x

if __name__ == "__main__":
    print get(1500)
    print get1(1500)

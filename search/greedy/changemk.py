#!/usr/bin/python

def change(x, coins):
    cs = {}
    while x != 0:
        m = max([c for c in coins if c <= x])
        cs[m] = 1 + cs.setdefault(m, 0)
        x = x - m
    return cs

USA = [1, 5, 25, 50, 100]

def test():
    print(change(142, USA))

if __name__ == "__main__":
    test()

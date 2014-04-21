#!/usr/bin/python

TAB = [1, 1]

def fibo(n):
    global TAB
    if n > len(TAB):
        TAB = TAB + [1] * n # expand the table
    if n > 2 and TAB[n] == 1:
        TAB[n] = fibo(n-1) + fibo(n-2)
    return TAB[n]

if __name__ == "__main__":
    print fibo(100)

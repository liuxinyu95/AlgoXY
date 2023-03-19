def pow(b, n):
    a = 1
    while n != 0:
        if n & 1 == 1:
            a = a * b
        b = b * b
        n = n >> 1
    return a

def test():
    for i in range(16):
        print("2^", i, "=", pow(2, i))

if __name__ == "__main__":
    test()

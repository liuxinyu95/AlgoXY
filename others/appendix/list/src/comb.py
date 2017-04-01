# Given a list of n elements, pick r from it to form combination.
# Enumerate all combinations of \binom{n}{r}

# DFS with recursion
def comb(xs, r):
    n = len(xs)
    acc = []
    def dfs(i, ys):
        if len(ys) == r:
            acc.append(ys)
        else:
            for j in range(i, n):
                dfs(j + 1, ys + [xs[j]])
    dfs(0, [])
    return acc

def test():
    xs = ['a', 'b', 'c', 'd']
    for r in range(6):
        print len(xs), "choose", r, ":", comb(xs, r)

if __name__ == "__main__":
    test()

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

# Another recursive solution
def combination(xs, r):
   if r <= 0 or r > len(xs):
       return []
   if (r == 1):
       return [[x] for x in xs]
   res = []
   for i in range(len(xs)):
       yss = combination(xs[i + 1 : ], r - 1)
       res = res + [[xs[i]] + ys for ys in yss]
   return res

def test():
    xs = ['a', 'b', 'c', 'd']
    for r in range(6):
        print len(xs), "choose", r, ":", comb(xs, r)
        print len(xs), "choose", r, ":", combination(xs, r)

if __name__ == "__main__":
    test()

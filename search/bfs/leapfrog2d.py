# Problem reference:
# Mathematical puzzles of Sam Loyd, selected and edited by Martin Gardner. 1959 by Dover Publications, Inc.
# Chinese translation:
# Chen Weipeng,
# Shanghai scientific & technological education publish house. 1999
# ISBN 7-5428-1928-3

# board layout:

# B, B, B
# B, B, B
# B, B, *
#       W, W, W
#       W, W, W
#       W, W, W

# where B is black piece, W is white piece, * is empty cell.
# rules: leap or hop to exchange all the blacks and whites
# the movement can only happen either vertically or horizontally, but not in diagonal direction.

def solve(i, j, start, end):
    stack = [[((i, j), start)]]
    s = []
    while stack != []:
        c = stack.pop()
        if c[0] == ((i, j), end):
            s.append([b for (_, b) in c][::-1])
            return s # find the 1st
        else:
            for m in moves(c[0]):
                stack.append([m] + c)
    return s

DELTA = [(0, 1), (1, 0), (0, -1), (-1, 0)]
def moves(s):
    ((i, j), m) = s
    return [((i+di, j+dj), swap(m, i, j, di, dj))
            for (di, dj) in (DELTA + [(dx*2, dy*2) for (dx, dy) in DELTA])
            if 0 <= i + di and i + di < 5 and 0 <= j + dj and j + dj < 5 and
               m[i+di][j+dj] == - (di+dj) / abs(di + dj)]

def swap(m, i, j, di, dj):
    a = [_[:] for _ in m]
    (a[i+di][j+dj], a[i][j]) = (a[i][j], a[i+di][j+dj])
    return a

CMAP = {0:"*", 2:" ", 1:"B", -1:"W"}
def output(ms):
    for m in ms:
        print "\n".join([" ".join([CMAP[x] for x in r]) for r in m]), "\n"

def test():
    start = [[1,  1,  1,  2,  2],
             [1,  1,  1,  2,  2],
             [1,  1,  0, -1, -1],
             [2,  2, -1, -1, -1],
             [2,  2, -1, -1, -1]]
    end = [[-x if abs(x) < 2 else x for x in r] for r in start]
    s = solve(2, 2, start, end)
    for m in s:
        output(m)
        print "total", len(m) - 1, "steps"
    print "total", len(s), "solutions"

if __name__ == "__main__":
    test()

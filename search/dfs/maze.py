#!/usr/bin/python

def solve(m, src, dst):
    stack = [[src]]
    s = []
    while stack != []:
        path = stack.pop()
        if path[-1] == dst:
            s.append(path)
        else:
            for p in adjacent(m, path[-1]):
                if not p in path:
                    stack.append(path + [p])
    return s

def adjacent(m, p):
    (x, y) = p
    ds = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    ps = []
    for (dx, dy) in ds:
        x1 = x + dx
        y1 = y + dy
        if 0 <= x1 and x1 < len(m[0]) and 0 <= y1 and y1 < len(m) and m[y][x] == 0:
            ps.append((x1, y1))
    return ps

def test():
    mz = [[0, 0, 1, 0, 1, 1],
          [1, 0, 1, 0, 1, 1],
          [1, 0, 0, 0, 0, 0],
          [1, 1, 0, 1, 1, 1],
          [0, 0, 0, 0, 0, 0],
          [0, 0, 0, 1, 1, 0]]
    mz1 = [[0, 0, 0, 0, 0, 1],
           [1, 0, 1, 1, 0, 1],
           [1, 0, 1, 1, 0, 1],
           [1, 0, 1, 1, 0, 1],
           [1, 0, 0, 0, 0, 0],
           [1, 1, 1, 1, 1, 0]]
    for m in [mz, mz1]:
        print(solve(m, (0, 0), (5,5)))

if __name__ == "__main__":
    test()

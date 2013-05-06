# saddleback.py
# Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Saddleback search based on Chapter 3 of [1]
# [1] Richard Bird. ``Pearls of functional algorithm design''. Cambridge University Press. 2010. ISBN, 1139490605, 9781139490603


# brute-force naive search
# In order to test easily, the brute force search actually starts from top-left corner, so that
# It finds the solution in the same order as saddleback search.

def brute_solve(f, z):
    return [(x, y) for x in range(z+1) for y in reversed(range (z+1)) if f(x, y) == z]

# Saddleback basic version based on [2]
# [2] Edsger W. Dijkstra. ``The saddleback search''. EWD-934. 1985. http://www.cs.utexas.edu/users/EWD/index09xx.html.

def saddleback(f, z):
    (p, q) = (0, z)
    res = []
    while p <= z and q >= 0:
        z1 = f(p, q)
        if z1 < z:
            p = p + 1
        elif z1 > z:
            q = q - 1
        else:
            res.append((p, q))
            (p, q) = (p + 1, q - 1)
    return res

def bsearch(f, z, l, u):
    while u > l:
        m = (l + u) // 2
        if f(m) <= z:
            l = m + 1
        else:
            u = m
    return l

def saddleback1(f, z):
    m = bsearch(lambda y: f(0, y), z, 0, z)
    n = bsearch(lambda x: f(x, 0), z, 0, z)
    res = []
    (p, q) = (0, m)
    while p <= n and q >= 0:
        z1 = f(p, q)
        if z1 < z:
            p = p + 1
        elif z1 > z:
            q = q - 1
        else:
            res.append((p, q))
            (p, q) = (p + 1, q - 1)
    return res

def test_search(search1, search2):
    fs = [lambda x, y: x + y, lambda x, y: pow(2, x) + pow(3, y), lambda x, y: x*x + y*y]
    for z in range(100+1):
        for f in fs:
            assert search1(f, z) == search2(f, z)

def test():
    test_search(brute_solve, saddleback)
    test_search(saddleback, saddleback1)

if __name__ == "__main__":
    test()

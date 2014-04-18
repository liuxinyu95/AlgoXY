#!/usr/bin/python

# changemk.py
# Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
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


tab = [[] for _ in range(1000)]

def change(x, cs):
    if x == 0 or tab[x] != []:
        return tab[x]
    a = None
    for s in [[c] + change(x - c, cs) for c in cs if c <= x]:
        if a is None or len(s) < len(a):
            a = s
    tab[x] = a
    return tab[x]

def test():
    USA = [1, 5, 25, 50, 100]
    print(change(142, USA))

if __name__ == "__main__":
    test()

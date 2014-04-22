#!/usr/bin/python

# fibo.py
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


TAB = [1, 1]

# top-bottom recursive solution
def fibo(n):
    global TAB
    if n > len(TAB):
        TAB = TAB + [1] * n # expand the table
    if n > 2 and TAB[n] == 1:
        TAB[n] = fibo(n-1) + fibo(n-2)
    return TAB[n]

# bottom-up solution
def fibonacci(n):
    tab = [1]*n
    for i in range(2, n):
        tab[i] = tab[i-1] + tab[i-2]
    return tab[n-1]

if __name__ == "__main__":
    print fibo(100)
    print fibonacci(100)

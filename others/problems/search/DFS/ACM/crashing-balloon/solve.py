#!/usr/bin/python

# solve.py
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


# ACM/ICPC, ZOJ 1003
# [1]. http://acm.zju.edu.cn/onlinejudge/showProblem.do?problemCode=1003

# Soluton 1: 
#   Suppose a > b, 
#   for any valide factor decomposition of b, there exists a valid decomposition of a => a
#   Otherwise => b
#
# Note that the ZOJ doesn't use this rule, solution 1 won't pass online judge, but I do
#    Think solution 1 has its value.

import sys

def valid(a, b, n):
    if n == 1:
        return a == 1 and b != 1
    else:
        if b % n == 0 and not valid(a, b/n, n-1):
            return False
        elif a % n == 0 and valid(a/n, b, n-1):
            return True
        else:
            return valid(a, b, n-1)
            
# Solution 2: (This can pass ZOJ)
#   If there exists a decomposition for both a and b ==> a
#   else if there exists a decomposition for b ==> b
#   otherwise ==> a

def valid2(a, b, n):
    return Judge().result(a, b, n)

class Judge:
    def __init__(self):
        self.both_OK = False
        self.b_OK = False

    def exist(self, a, b, n):
        if self.both_OK:
            return True

        if a == 1 and b==1:
            (self.both_OK, self.b_OK) = (True, True)
            return True

        if b == 1:
            self.b_OK = True

        for x in xrange(n, 1, -1):
            if b % x == 0 and self.exist(a, b/x, x-1):
                return True
            if a % x == 0 and self.exist(a/x, b, x-1):
                return True

        return self.both_OK

    def result(self, a, b, n):
        self.exist(a, b, n)
        return not ( self.b_OK and not self.both_OK)

def main():
    for line in sys.stdin:
        [s1, s2] = line.split()
        (a, b) = (int(s1), int(s2))
        if a < b:
            (a, b) = (b, a)
        if valid2(a, b, 100):
            print a
        else:
            print b

if __name__ == "__main__":
    main()

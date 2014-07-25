#!/usr/bin/python

# measure.py, measure the performance of the algorithms
# Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)
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

import time
import random
import minfree

# return the second elapsed
def measure(f, x):
    start = time.time()
    f(x)
    end = time.time()
    return end - start

def test_min_free(f):
    lst = range(100000) #0.1 million
    tm = 0
    for i in range(100):
        random.shuffle(lst)
        x = lst.pop()
        tm = tm + measure(f, lst)
        print tm, "[s] elapsed."
        lst.append(x)
    print "average time", tm/100, "[s]"

def test():
    #test_min_free(minfree.brute_force)
    #test_min_free(minfree.min_free) 
    test_min_free(minfree.dc_min_free) 

# ============================
# Some performance data
#
# N = 10,000,000      10 million
# Brute force:
# Flags method: 4 [sec]
# Divide and Conquer: 10 [sec]
#
# N = 100,000
# Brute Froce:
# Flags method: 0.0199500107765 [s]
# Divide and Conquer: 0.0445399999619 [s]
#
# ============================

if __name__ == "__main__":
    test()

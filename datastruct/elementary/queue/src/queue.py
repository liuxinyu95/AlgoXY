#!/usr/bin/python

# queue.py, batched queue
# Copyright (C) 2012, Liu Xinyu (liuxinyu95@gmail.com)
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

#
# Batched queue based on Based on Chris Okasaki, 
#   ``Purely Functional Datastructures''
#

import random # for testing only

class Queue:
    def __init__(self):
        self.front = []
        self.rear = []

def is_empty(q):
    return q.front == [] and q.rear == []

# O(1) time push
def push(q, x):
    q.rear.append(x)

def pop(q):
    if q.front == []:
        q.rear.reverse()
        (q.front, q.rear) = (q.rear, [])
    return q.front.pop()

# testing

def test():
    n = 1000
    for i in range(n):
        lst = random.sample(range(n), random.randint(1, n))
        q = Queue()
        l = []
        for x in lst:
            if x % 2 == 0:
                push(q, x)
                l.append(x)
            elif not is_empty(q):
                assert(l.pop(0) == pop(q))
    print "Batched Queue:", n, "test cases are OK."        
    
if __name__ == "__main__":
    test()

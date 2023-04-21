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

from random import sample # for verification

# Batched queue based on paired array (front: f, rear: r) [1].
#    Push new element to the tail of front;
#    Pop from the tail of rear.

class Queue:
    def __init__(self):
        self.front = []
        self.rear = []

    def is_empty(self):
        return not self.front

    def push(self, x):
        self.front.append(x)

    def pop(self):
        if not self.rear:
            self.rear = list(reversed(self.front))
            self.front = []
        return self.rear.pop()

    def to_list(self):
        return list(reversed(self.rear)) + self.front

def test():
    n = 100
    q = Queue()
    xs = []
    for x in sample(range(n), n):
        if x % 2 == 0:
            q.push(x)
            xs.append(x)
        elif not q.is_empty():
            a = xs.pop(0)
            b = q.pop()
            assert a == b, f"pop fail: list pop: {a}, queue pop: {b}"
        ys = q.to_list()
        assert xs == ys, f"error: list = {xs}, queue = {ys}"
    print(f"Batched Queue {n} test cases passed.")

if __name__ == "__main__":
    test()

# [1] Chris Okasaki. ``Purely Functional Data Structures.'' Cambridge university press, (July 1, 1999), ISBN-13: 978-0521663502

# Copyright (C) 2023, Liu Xinyu (liuxinyu95@gmail.com)
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

# Realtime queue based on paired array (front: f, rear: r).
#    Push new element to the tail of front;
#    Pop from the tail of rear.

# As the len() function is O(1) for native list, we needn't maintain the
# length of front/rear during push/pop
class State:
    def __init__(self, f = [], r = []):
        self.acc = []
        self.front = f
        self.rear = r
        self.idx = 0
        self.step()

    # compute reverse(f) ++ r step by step
    def step(self):
        if self.front:    # reversing
            self.acc.append(self.front.pop())
        if (not self.front) and self.idx < len(self.rear):    # concatenating
            self.acc.append(self.rear[self.idx])
            self.idx = self.idx + 1
        return self

    def done(self):
        return (not self.front) and len(self.rear) <= self.idx

    def to_list(self):
        return self.front + (list(reversed(self.acc))[self.idx:])

    def __str__(self):
        return f"acc:{self.acc}, f:{self.front}, r:{self.rear}, idx:{self.idx}"

class RealtimeQueue:
    def __init__(self):
        self.front = []
        self.rear = []
        self.state = None

    def is_empty(self):
        return (not self.front) and (not self.rear)

    def push(self, x):
        self.front.append(x)
        self.balance()

    def pop(self):
        x = self.rear.pop()
        self.balance()
        return x

    def balance(self):
        if not self.state and len(self.rear) < len(self.front):
            self.state = State(self.front, self.rear)
            self.front = []
        if self.state and self.state.step().done():
            self.rear = self.state.acc
            self.state = None

    def to_list(self):
        return list(reversed(self.rear)) + \
            (self.state.to_list() if self.state else []) + \
            self.front

    def __str__(self):
        return f"f: {self.front}, r: {self.rear}, st: {self.state}"

def test():
    n = 100
    q = RealtimeQueue()
    xs = []
    for x in sample(range(n), n):
        if x % 2 == 0:
            q.push(x)
            xs.append(x)
        else:
            assert q.is_empty() == (xs == []), f"empty test fail: xs: {xs}, q: {q}"
            if not q.is_empty():
                a = xs.pop(0)
                b = q.pop()
                assert a == b, f"pop fail: list pop: {a}, queue pop: {b}"
        ys = q.to_list()
        assert xs == ys, f"error: list = {xs}, queue = {ys}, ({q})"
    print(f"Batched Queue {n} test cases passed.")

if __name__ == "__main__":
    test()

# [1] Chris Okasaki. ``Purely Functional Data Structures.'' Cambridge university press, (July 1, 1999), ISBN-13: 978-0521663502

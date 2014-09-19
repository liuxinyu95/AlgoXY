# palindrome.py
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

# Suppose '#' isn't in the string
DELIMITER = "#"

# The brute force solution. O(n^2)
def brute_force_palindrome(s):
    s = DELIMITER + DELIMITER.join(s) + DELIMITER
    n = len(s)
    p = [0] * n
    for i in xrange(n):
        while 0 <= i-p[i] and i+p[i] < n and s[i-p[i]] == s[i+p[i]]:
            p[i] = p[i] + 1
    return max(p) - 1

# Manacher's algorithm, linear O(n) time.
def manacher_palindrome(s):
    s = DELIMITER + DELIMITER.join(s) + DELIMITER
    n = len(s)
    p = [0] * n
    j = 0
    m = (-1, 0)  # (max length so far, position)
    for i in xrange(n):
        p[i] = min(p[2*j-i], j + p[j] - i) if i < j + p[j] else 1
        while 0 <= i - p[i] and i + p[i] < n and s[i-p[i]] == s[i+p[i]]:
            p[i] = p[i] + 1
        if j + p[j] < i + p[i]:
            j = i
        m = max(m, (p[i] - 1, i))
    return m

def test():
    ss = ["Mississippi", "level", "cacab", "cocoa", "aaa", "abc"]
    for s in ss:
        (n, i) = manacher_palindrome(s)
        assert(n == brute_force_palindrome(s))
        print s[(i-n+1)/2 : (i+ n-1)/2+1]

if __name__ == "__main__":
    test()

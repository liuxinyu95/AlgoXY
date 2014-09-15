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
        k = 0
        while 0 <= i-k and i+k < n and s[i-k] == s[i+k]:
            p[i] = p[i] + 1
            k = k + 1
    return max(p) - 1

if __name__ == "__main__":
    print brute_force_palindrome("Mississippi")

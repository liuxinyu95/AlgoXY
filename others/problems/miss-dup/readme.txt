# Missing and Duplicated puzzle

Given numbers from 1 to n, after some processing, there are some changes. 1) The order is shuffled; 2) One number x is mutated to y, here both x, y are from 1 to n. Develop a method to find the x and y in linear time with constant space.

Examples
[3, 1, 3, 5, 4] ==> x = 2, y = 3

## Divide and Conquer

We can divide the list with the middle number m = floor((1 + n) / 2). Move all the numbers <= m to the left; and the rest to the right.

If the length of left < m, we immediately know that the missing number is on the left. denote s = 1 + 2 + ... + m = m(m + 1)/2, then x = s - sum(left). At the same time, we know that, the duplicated numbers are on the right. Denote s' = (m + 1) + (m + 2) + ... + n = (n + m + 1)(n - m)/2, then y = sum(right) - s';

If the length of left > m, we immediately know that the duplicated number is on the left. Use the similiar method, we have the missign number x = s' - sum(right), and the duplicated number y = sum(left) - s

Otherwise, it means the length of left is equal to m. We know that there are m numbers not greater than m, but we don't know if they are some permutation of 1, 2, ..., m. We can further compare between sum(left) and s. if they are equal, we know the left side is OK, we can drop the whole left side, and recursively find x, y on the right; Otherwise, we drop the right and recursivey find the answer on the left.

During the recursion, we need change 1 in the above discussion to the lower bound of the list. Because each time we halve the list, so the total time, according to the Master Theory, is O(n). Alternatively, we can deduce the time as O(n + n/2 + n/4 + ...) = O(2n) = O(n).

Below are the example code in Haskell and Python.

    def solve(xs):
        l = 0
        u = len(xs)
        while l<u:
            m = (l+u)/2
            left = [x for x in xs if x < m]
            right = [x for x in xs if x >= m]
            if len(left) < m - l:
                lost = (m - 1 + l)*(m - l)/2 - sum(left)
                dup  = sum(right) - (u - 1 + m)*(u - m)/2
                return (lost, dup)
            elif len(left) > m - l:
                lost = (u - 1 + m)*(u - m)/2 - sum(right)
                dup  = sum(left) - (m - 1 + l)*(m - l)/2
                return (lost, dup)
            else:
                if sum(left) == (m - 1 + l)*(m - l)/2:
                    l = m
                    xs = right
                else:
                    u = m
                    xs = left

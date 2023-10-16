{-
    Q235.hs, find the n-th number which only contains factors of 2,3,5.
    Copyright (C) 2010, Liu Xinyu (liuxinyu99@hotmail.com)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Q235 where

import Data.List (minimum)

-- method 1, brute force:
merge (x:xs) (y:ys) | x <y = x : merge xs (y:ys)
                    | x ==y = x : merge xs ys
                    | otherwise = y : merge (x:xs) ys

ns = 1 : (map (*2) ns) `merge` (map (*3) ns) `merge` (map (*5) ns)

-- ns !! 1500

xs = 1 : [2*x | x <- xs] `merge` [3*x | x <- xs] `merge` [5*x | x <- xs]

-- method 2, three queues
rs 0 xs _ _ _ = reverse xs
rs n xs q2 q3 q5 = rs (n - 1) (x : xs) q2' q3' q5'
    where
      x = minimum $ map head [q2, q3, q5]
      (q2', q3', q5')
        | x == head q2 = ((tail q2) ++ [2 * x], q3 ++ [3 * x], q5 ++ [5 * x])
        | x == head q3 = (q2, (tail q3) ++ [3 * x], q5 ++ [5 * x])
        | otherwise = (q2, q3, (tail q5) ++ [5 * x])

-- rs 1500 [1] ([2], [3], [5])

test n = rs n [1] [2] [3] [5] == take (n + 1) ns

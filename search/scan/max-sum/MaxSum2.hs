-- 2D max-sum
-- Given a matrix of m*n integers, find the sub matrix with maximum sum
-- of its elements

module MaxSum2 where

import Data.List (scanl1, inits, tails)

maxSum = maximum . (map maxS) . concat . acc . rows where
    rows = init . tails -- exclude the empty row
    acc = map (scanl1 (zipWith (+))) -- accumulated sum along columns
    maxS = snd . (foldl f (0, 0)) -- max sum in a vector
    f (m, s) x = let m' = max (m + x) 0
                     s' = max m' s in (m', s')

example = [[1, 2, -1, -4, -20],
           [-8, -3, 4, 2, 1],
           [3, 8, 10, 1, 3],
           [-4, -1, 1, 7, -6]]
-- maxSum example ==> 29

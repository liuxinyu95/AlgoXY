module MissDup where

import Data.List (partition)
import Test.QuickCheck

-- Divide and Conquer

missDup xs = solve xs 1 (toInteger $ length xs)

solve xs@(_:_:_) l u | k < m - l + 1 = (sl - sl', sr' - sr)
                     | k > m - l + 1 = (sr - sr', sl' - sl)
                     | sl == sl' = solve bs (m + 1) u
                     | otherwise = solve as l m
  where
    m = (l + u) `div` 2
    (as, bs) = partition (<=m) xs
    k = toInteger $ length as
    sl = (l + m) * (m - l + 1) `div` 2
    sr = (m + 1 + u) * (u - m) `div` 2
    (sl', sr') = (sum as, sum bs)

-- Verification
xs = [3, 1, 2, 2, 5]

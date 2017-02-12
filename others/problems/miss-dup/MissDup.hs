module MissDup where

-- Given numbers from 1 to n, after some processing, there are some changes.
--   1) The order is shuffled;
--   2) One number x is mutated to y, here both x, y are from 1 to n.
-- Develop a method to find the x and y in linear time with constant space.

-- Examples
-- [3, 1, 3, 5, 4] ==> x = 2, y = 3


import Data.List (partition)
import Test.QuickCheck

-- Divide and Conquer

missDup xs = solve xs 1 (toInteger $ length xs) where
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

data Sample = Sample [Integer] Integer deriving (Show)

instance Arbitrary Sample where
  arbitrary = do
    n <- choose (2, 100)
    (x:xs) <- shuffle [1..n]
    y <- elements xs
    return $ Sample (y:xs) x


prop_solve :: Sample -> Bool
prop_solve (Sample xs@(y:_) x) = let (x', y') = missDup xs in x == x' && y == y'

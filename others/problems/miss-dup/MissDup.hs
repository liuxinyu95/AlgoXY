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

-- Equations
missDup' xs = ((b `div` a - a) `div` 2, (b `div` a + a) `div` 2) where
  ys = zip xs [1..]
  a = sum [x - y | (x, y) <- ys]
  b = sum [x^2 - y^2 | (x, y) <- ys]

-- Verification

data Sample = Sample [Integer] (Integer, Integer) deriving (Show)

instance Arbitrary Sample where
  arbitrary = do
    n <- choose (2, 100)
    (x:y:xs) <- shuffle [1..n]
    xs' <- shuffle (y:y:xs)
    return $ Sample xs' (x, y)

prop_solve :: Sample -> Bool
prop_solve (Sample xs (x, y)) = let (x', y') = missDup xs in x == x' && y == y'

prop_equation :: Sample -> Bool
prop_equation (Sample xs (x, y)) = let (x', y') = missDup' xs in x == x' && y == y'

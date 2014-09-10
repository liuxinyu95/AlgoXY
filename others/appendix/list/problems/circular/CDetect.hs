module CDetect where

import Test.QuickCheck

-- Cycle detection problem

-- Method 1, Robert W. Floyd's algorithm
findCycle x0 f = (k, n) where
  (k, p) = connect x0 (converge (f x0) (f $ f x0)) 0
  n = traverse (f p)
  converge a b | a == b = a
               | otherwise = converge (f a) (f $ f b)
  connect a b cnt | a == b = (cnt, a)
                  | otherwise = connect (f a) (f b) (cnt + 1)
  traverse a | a == p = 1
             | otherwise = 1 + traverse (f a)


-- Floyd's method by using infinity
findCycle' x0 f = (length sec, length cycle) where
  xs@(x:xs') = iterate f x0
  ys@(y:ys') = iterate (f.f) x0
  neq (x, y) = x /= y
  converge = fst $ unzip $ dropWhile neq (zip xs' ys')
  (sec, (z:zs)) = span neq (zip xs converge)
  cycle = z : takeWhile (z /=) zs

-- Mehod 2, Richard P. Brent's algorithm
detectCycle x0 f = (k, n) where
  n = converge x0 (f x0) 1 1
  q = foldr ($) x0 (replicate n f)
  k = connect x0 q
  connect p q | p == q = 0
              | otherwise = 1 + connect (f p) (f q)
  converge p q n m | n == m = converge q (f q) 1 (2*m)
                   | p == q = n
                   | otherwise = converge p (f q) (n + 1) m

-- Verification

-- Specified function f on non-negative integers.
f k m n | n < k = n + 1
        | otherwise = (n + 1 - k) `mod` m + k

prop_cycle fcycle b c = (k, m) == fcycle 0 (f k m) where
  (k, m) = ((abs b) `mod` 1000, max 1 (abs c) `mod` 1000)   -- limit to 1000 to save the time

prop_floyd :: Int -> Int -> Bool
prop_floyd = prop_cycle findCycle

prop_floyd' :: Int -> Int -> Bool
prop_floyd' = prop_cycle findCycle'

prop_brent :: Int -> Int -> Bool
prop_brent = prop_cycle detectCycle

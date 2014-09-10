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
--findCycle' x0 f = (k, n) where

-- Verification
f k m n | n < k = n + 1
        | otherwise = (n + 1 - k) `mod` m + k

prop_cycle :: Int -> Int -> Bool
prop_cycle b c = (k, m) == findCycle 0 (f k m) where
  k = (abs b) `mod` 1000
  m = if c == 0 then 1 else (abs c) `mod` 1000

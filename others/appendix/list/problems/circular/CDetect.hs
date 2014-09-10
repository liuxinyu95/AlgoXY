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

-- Verification
f k m n | n < k = n + 1
        | otherwise = (n + 1 - k) `mod` m + k

-- limit the k, m < 1000 to save the running time
norm k m = ((abs k) `mod` 1000, max 1 (abs m) `mod` 1000)

prop_floyd :: Int -> Int -> Bool
prop_floyd b c = (k, m) == findCycle 0 (f k m) where
  (k, m) = norm b c

prop_floyd' :: Int -> Int -> Bool
prop_floyd' b c = (k, m) == findCycle' 0 (f k m) where (k, m) = norm b c

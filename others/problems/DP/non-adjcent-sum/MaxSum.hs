-- Max sum of non-adjacent elements

module MaxSum where

import Data.List (zipWith3)
import Test.QuickCheck

-- top-down recursive method
--  This might be slow for long list, as there are many duplicated calculation
maxsum [] = 0
maxsum [x] = x
maxsum (x:y:xs) = max (x + maxsum xs) (maxsum (y:xs))

-- iterative method by folding
maxsum2 xs = max a b where
  (a, b) = foldl (\(i, e) x -> (x + e, max i e)) (0, 0) xs

-- lazy infinite recursive definition similiar to Fibonacci
maxsums [] = [0]
maxsums [x] = [0, x]
maxsums [x, y] = [0, x, max x y]
maxsums (x:y:xs) = ms where
  ms = 0 : x : (max x y) : zipWith3 (\x' m m' -> max (x' + m) m') xs (tail ms) (drop 2 ms)

prop_maxsum :: [Int] -> Bool
prop_maxsum xs = let ns = take 20 $ map (abs . (`mod` 100)) xs in maxsum ns == maxsum2 ns

prop_maxsums :: [Int] -> Bool
prop_maxsums xs = let ns = map (abs . (`mod` 100)) xs in maxsum2 ns == (last $ maxsums ns)

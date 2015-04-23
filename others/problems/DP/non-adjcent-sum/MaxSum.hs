-- Max sum of non-adjacent elements

module MaxSum where

import Test.QuickCheck

-- top-down recursive method
--  This might be slow for long list, as there are many duplicated calculation
maxsum [] = 0
maxsum [x] = x
maxsum (x:y:xs) = max (x + maxsum xs) (maxsum (y:xs))

-- iterative method by folding
maxsum2 xs = max a b where
  (a, b) = foldl (\(i, e) x -> (x + e, max i e)) (0, 0) xs

prop_maxsum :: [Int] -> Bool
prop_maxsum xs = let ns = take 20 $ map (abs . (`mod` 100)) xs in maxsum ns == maxsum2 ns

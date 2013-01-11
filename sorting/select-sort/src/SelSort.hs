module SelSort where
  
import Test.QuickCheck
import Data.List

ssort [] = []
ssort xs = x : ssort xs' where
  (x, xs') = extractMin xs
  
extractMin (x:xs) = min' [] x xs where
  min' ys m [] = (m, ys) -- Note: ys is in reversed order!
  min' ys m (x:xs) = if m < x then min' (x:ys) m xs else min' (m:ys) x xs
  
ssort' [] = []
ssort' xs = x : ssort' xs' where
  (x, xs') = extractMin' xs

extractMin' [x] = (x, [])
extractMin' (x:xs) = if x < m then (x, xs) else (m, x:xs') where
  (m, xs') = extractMin' xs
  
prop_ssort :: [Int] -> Bool
prop_ssort xs = (sort xs) == (ssort xs)

prop_ssort' :: [Int] -> Bool
prop_ssort' xs = (sort xs) == (ssort' xs)

module BSort where

import Test.QuickCheck
import qualified Data.List as L

pass [] = (True, [])
pass [x] = (True, [x])
pass (x:y:xs) | x <= y    = let (o, xs') = pass (y:xs) in (o, x:xs')
              | otherwise = let (_, xs') = pass (x:xs) in (False, y:xs')

bsort xs = let (o, xs') = pass xs in
           if o then xs' else bsort xs'

prop_sort :: (Ord a, Num a) => [a] -> Bool
prop_sort xs = L.sort xs == bsort xs
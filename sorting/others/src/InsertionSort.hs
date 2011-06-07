module InsertionSort where

import Test.QuickCheck
import qualified Data.List as L

isort [] = []
isort (x:xs) = insert x $ isort xs where
    insert x [] = [x]
    insert x (y:ys) = if x < y then x:y:ys else y:insert x ys

prop_sort :: (Ord a, Num a) => [a] -> Bool
prop_sort xs = L.sort xs == isort xs
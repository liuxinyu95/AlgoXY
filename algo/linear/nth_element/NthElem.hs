module NthElem where

import Data.List
import Test.QuickCheck

top::Int->[Int]->[Int]
top _ [] = []
top 0 _  = []
top n (x:xs) | len ==n = as
             | len < n  = as++[x]++(top (n-len-1) bs)
             | otherwise = top n as
    where
      (as, bs) = partition (<= x) xs
      len = length as

prop_top :: Int ->[Int] ->Bool
prop_top k xs = if k>=0 then sort (top k xs) == take k (sort xs)
                else True
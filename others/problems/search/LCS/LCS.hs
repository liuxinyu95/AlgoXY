-- Longest Common Subsequence for two sequences.

module LCS where

import Data.Function (on)
import Data.List (maximumBy, subsequences, intersect)
import Test.QuickCheck

lcs :: (Eq a) => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs (x:xs) (y:ys) | x == y = x : lcs xs ys
                  | otherwise = maximumBy (compare `on` length) [lcs xs (y:ys), lcs (x:xs) ys]


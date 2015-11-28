module InterLists where

import Data.List
import Test.QuickCheck

-- Find the LCA (Least Common Ancestor) of two lists.
-- The list can represent the back-trace path from a node to the root.
-- Refer to LCA prlblem for more general topic (find LCA of 2 nodes in tree or DAG)

lca xs ys = let (m, n) = (length xs, length ys) in
            if m < n then match xs $ drop (n - m) ys
            else match (drop (m - n) xs) ys

match [] [] = Nothing
match (a:as) (b:bs) | a == b = Just a
                    | otherwise = match as bs

-- test
prop_lca :: [Int] -> Int -> Int -> Bool
prop_lca xs n m = lca ys zs == if suffix == [] then Nothing else Just (head suffix)
    where
      xs' = nub (0:xs)
      n' = (abs n) `mod` (length xs')
      (asbs, suffix) = splitAt (1 + n') xs'
      m' = (abs m) `mod` (length asbs)
      (as, bs) = splitAt m' asbs
      (ys, zs) = (as ++ suffix, bs ++ suffix)
      r = lca ys zs

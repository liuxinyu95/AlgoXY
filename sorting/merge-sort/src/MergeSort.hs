-- MergeSort1.hs
-- Copyright (C) 2012 Liu Xinyu (liuxinyu95@gmail.com)
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module MergeSort where

import Test.QuickCheck
import Data.List

-- Basic version, split at the middle point
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs) where
  (as, bs) = splitAt (length xs `div` 2) xs


-- Basic version, split odd/even

mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort as) (mergesort bs) where
    (as, bs) = split xs

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:xs', y:ys') where (xs', ys') = split xs

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x >  y = y : merge (x:xs) ys
                               
-- Bottom up version
-- Based on: Chris Okasaki. ``Purely functional data structures'' 6.4.3. 
bmsort = sort' . map (\x->[x])

sort' [] = []
sort' [xs] = xs
sort' xss = sort' (mergePairs xss) where
    mergePairs (xs:ys:xss) = merge xs ys : mergePairs xss
    mergePairs xss = xss

-- Bottom up version using foldl
bmsort' = foldl merge [] . map (\x->[x]) where

-- A version for performance visualization
-- This version record the number of swaps which happens during sorting.

--merge :: (Ord a)=>([a], Int)->([a], Int) -> ([a], Int)
merge' ([], n) (ys, m) = (ys, n+m)
merge' (xs, n) ([], m) = (xs, n+m)
merge' ((x:xs), n) ((y:ys), m) = 
    if x< y then let (xs', n') = merge' (xs, n) ((y:ys), m) in (x:xs', n')
    else let (xs', n') = merge' ((x:xs), n) (ys, m) in (y:xs', n'+1)

--sort'' :: (Ord a)=>[([a], Int)] -> ([a], Int)
sort'' [] = ([], 0)
sort'' [(xs, n)] = (xs, n)
sort'' xss = sort'' $ mergePairs' xss where
    mergePairs' (xs:ys:xss) = merge' xs ys : mergePairs' xss
    mergeParis' xss = xss


msort' :: (Ord a)=>[a]->([a], Int)
msort' = sort'' . map (\x->([x], 0))

--test
prop_msort :: [Int] -> Bool
prop_msort xs = sort xs == (msort xs)

prop_mergesort :: [Int] -> Bool
prop_mergesort xs = sort xs == (mergesort xs)

prop_bmsort :: [Int] -> Bool
prop_bmsort xs = sort xs == (bmsort xs)

prop_bmsort' :: [Int] -> Bool
prop_bmsort' xs = sort xs == (bmsort' xs)
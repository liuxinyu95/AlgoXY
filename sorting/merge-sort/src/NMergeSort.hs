-- NMergeSort.hs
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

-- Nature merge sort
--  [1], Donald E Knuth, `The art of computer programming, Volume 3, sorting and searching'

module NMergeSort where

import Data.List
import Test.QuickCheck
import MergeSort (merge, sort')

-- Nature merge sort can be realized as a genericl ver. of bottom-up merge sort
mergesort = sort' . groupBy' (<=)

--mergesort' :: (Ord a) => [a] -> [a]
mergesort' = foldl merge [] . groupBy' (<=)
  
-- groupBy in Data.List doesn't fit here, because it use eq funciton
--  which must satisfy: reflexive, transitive, and symmetric,
--  but (<=) only satisfy transitive
groupBy' :: (a->a->Bool) ->[a] ->[[a]]
groupBy' _ [] = [[]]
groupBy' _ [x] = [[x]]
groupBy' f (x:xs@(x':_)) | f x x' = (x:ys):yss
                         | otherwise = [x]:r
  where
    r@(ys:yss) = groupBy' f xs

prop_sort :: [Int] -> Bool
prop_sort xs = mergesort xs == sort xs

prop_sort' :: [Int] -> Bool
prop_sort' xs = mergesort' xs == sort xs
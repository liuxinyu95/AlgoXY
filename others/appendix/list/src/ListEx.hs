-- ListEx.hs
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

module ListEx where

import Data.List
import Test.QuickCheck -- for verification purpose only

atR :: [a] -> Int -> a
atR xs i = get xs (drop i xs) where
  get (x:_) [_] = x
  get (_:xs) (_:ys) = get xs ys

insertAt :: [a] -> Int -> a -> [a]
insertAt xs 0 y = y:xs
insertAt [] i y = [y]
insertAt (x:xs) i y = x : insertAt xs (i-1) y

permutation xs n r=  if length xs  <=  n - r
        then [[]]
        else [x:ys | x  <-  xs, ys  <-  permutation (delete x xs) n r]

-- Given a list of n elements, pick r from it to for permutation.
perm _ 0 = [[]]
perm xs r | length xs < r = [[]]
          | otherwise = [ x:ys | x <-xs, ys <- perm (delete x xs) (r-1)]

-- Given a list of n elements, pick r from it to form combination.
comb _  0 = []
comb xs 1 = [[x] | x <- xs]
comb xs@(x:xs') r | length xs < r = []
                  | otherwise = [x:ys | ys <- comb xs' (r-1)] ++ comb xs' r

prop_rindex :: [Int] -> Bool
prop_rindex xs = xs == (map (atR xs) $ reverse [0..length xs -1])

prop_insertAt :: [Int] -> Int -> Int -> Property
prop_insertAt xs i x = (0 <= i) ==> (insertAt xs i x) == (let (as, bs) = splitAt i xs in as ++ x:bs)

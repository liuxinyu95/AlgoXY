-- Subsetsum.hs
-- Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
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

module Subsetsum where

import Data.List (subsequences, nub)
import Data.Sequence (Seq, fromList, index, adjust)
import Test.QuickCheck

naiveSolve xs n = filter (\ys->n == sum ys) $ tail $ subsequences xs

solve :: (Num a, Eq a)=>[a] -> a -> [[a]]
solve [] n = []
solve (x:xs) n = if x == n then [x]:xss else xss where
    xss = solve xs n ++ map (x:) (solve xs (n-x))

-- Bottom-up dynamic programing solution with finger tree
subsetsum xs s | xs ==[] || s < l || s > u = []
               | otherwise = foldl build (fromList [[] | _ <- [l..u]]) xs `idx` s where
  l = sum $ filter (< 0) xs
  u = sum $ filter (> 0) xs
  idx t i = index t (i - l)
  build tab x = foldl (\t j -> let j' = j - x in
                        adjustIf (l <= j' && j' <= u && tab `idx` j' /= [])
                            (++ [(x:ys) | ys <- tab `idx` j']) j
                            (adjustIf (x == j) ([x]:) j t)) tab [l..u]
  adjustIf pred f i seq = if pred then adjust f (i - l) seq else seq

example = subsetsum [11, 64, -82, -68, 86, 55, -88, -21, 51] 0

-- test.
prop_subsetsum :: [Int] -> Int -> Bool
prop_subsetsum xs s = all (\ys -> s' == sum ys) $ subsetsum xs' s' where
  s' = s `mod` 100
  xs' = nub $ map (`mod` 100) xs

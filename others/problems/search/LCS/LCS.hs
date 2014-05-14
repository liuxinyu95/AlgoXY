-- LCS.hs
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

-- Longest Common Subsequence for two sequences.

module LCS where

import Data.Function (on)
import Data.List (maximumBy, subsequences, intersect)
import Data.Sequence (Seq, singleton, fromList, index, (|>))
import Test.QuickCheck

-- [1]. ``Longest common subsequence problem''.
--        http://en.wikipedia.org/wiki/Longest_common_subsequence_problem

-- Naive Top-down recursive method, note that there are a lot of
-- overlapping subproblems, so the efficiency is poor.

lcs [] _ = []
lcs _ [] = []
lcs (x:xs) (y:ys) | x == y = x : lcs xs ys
                  | otherwise = maximumBy (compare `on` length) [lcs xs (y:ys), lcs (x:xs) ys]

-- Bottom-up dynamic programming solution with finger tree
lcs' xs ys = construct $ foldl f (singleton $ fromList $ replicate (n+1) 0) (zip [1..] xs) where
  (m, n) = (length xs, length ys)
  f tab (i, x) = tab |> (foldl longest (singleton 0) (zip [1..] ys)) where
    longest r (j, y) = r |> if x == y then 1 + (tab `index` (i-1) `index` (j-1))
                            else max (tab `index` (i-1) `index` j) (r `index` (j-1))
  construct tab = get (reverse xs, m) (reverse ys, n) where
    get ([], 0) ([], 0) = []
    get ((x:xs), i) ((y:ys), j)
      | x == y = get (xs, i-1) (ys, j-1) ++ [x]
      | (tab `index` (i-1) `index` j) > (tab `index` i `index` (j-1)) = get (xs, i-1) ((y:ys), j)
      | otherwise = get ((x:xs), i) (ys, j-1)

example = lcs' "Mississippi" "Missunderstanding"

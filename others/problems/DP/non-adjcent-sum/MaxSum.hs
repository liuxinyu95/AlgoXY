-- MaxSum.hs
-- Copyright (C) 2015 Liu Xinyu (liuxinyu95@gmail.com)
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

-- Max sum of non-adjacent elements

module MaxSum where

import Data.List (zipWith3)
import Test.QuickCheck

-- top-down recursive method
--  This might be slow for long list, as there are many duplicated calculation
maxsum [] = 0
maxsum [x] = x
maxsum (x:y:xs) = max (x + maxsum xs) (maxsum (y:xs))

-- iterative method by folding
maxsum2 xs = max a b where
  (a, b) = foldl (\(i, e) x -> (x + e, max i e)) (0, 0) xs

-- lazy infinite recursive definition similiar to Fibonacci
maxsums [] = [0]
maxsums [x] = [0, x]
maxsums (x:xs) = ms where
  ms = 0 : x : zipWith3 (\x' m m' -> max (x' + m) m') xs ms (tail ms)

prop_maxsum :: [Int] -> Bool
prop_maxsum xs = let ns = take 20 $ map (abs . (`mod` 100)) xs in maxsum ns == maxsum2 ns

prop_maxsums :: [Int] -> Bool
prop_maxsums xs = let ns = map (abs . (`mod` 100)) xs in maxsum2 ns == (last $ maxsums ns)

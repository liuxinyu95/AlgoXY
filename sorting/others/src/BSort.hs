-- BSort.hs
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

-- Bubble Sort
module BSort where

import Test.QuickCheck
import qualified Data.List as L

pass [] = (True, [])
pass [x] = (True, [x])
pass (x:y:xs) | x <= y    = let (o, xs') = pass (y:xs) in (o, x:xs')
              | otherwise = let (_, xs') = pass (x:xs) in (False, y:xs')

bsort xs = let (o, xs') = pass xs in
           if o then xs' else bsort xs'

prop_sort :: (Ord a, Num a) => [a] -> Bool
prop_sort xs = L.sort xs == bsort xs

-- InsertionSort.hs
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

module InsertionSort where

import Test.QuickCheck
import qualified Data.List as L

insert :: (Ord a) => [a] -> a -> [a]
insert [] x = [x]
insert (y:ys) x = if x < y then x:y:ys else y:insert ys x

-- Version 1
isort [] = []
isort (x:xs) = insert (isort xs) x

-- Version 2
isort' :: (Ord a) => [a] -> [a]
isort' = foldl insert []

prop_sort :: (Ord a, Num a) => [a] -> Bool
prop_sort xs = L.sort xs == isort xs

prop_sort' :: (Ord a, Num a) => [a] -> Bool
prop_sort' xs = L.sort xs == isort' xs
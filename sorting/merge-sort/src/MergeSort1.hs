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

module MergeSort1 where

-- This is the basic version according to merge sort definition.

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

test = mergesort [5, 9, 1, 0, 5, 4, 3, 1, 2, 3]

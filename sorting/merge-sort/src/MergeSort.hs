-- MergeSort.hs
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

--merge :: (Ord a)=>([a], Int)->([a], Int) -> ([a], Int)
merge ([], n) (ys, m) = (ys, n+m)
merge (xs, n) ([], m) = (xs, n+m)
merge ((x:xs), n) ((y:ys), m) = 
    if x< y then let (xs', n') = merge (xs, n) ((y:ys), m) in (x:xs', n')
    else let (xs', n') = merge ((x:xs), n) (ys, m) in (y:xs', n'+1)

--sort' :: (Ord a)=>[([a], Int)] -> ([a], Int)
sort' [] = ([], 0)
sort' [(xs, n)] = (xs, n)
sort' xss = sort' $ mergePairs xss

--mergePairs :: (Ord a)=>[([a], Int)]->[([a], Int)]
mergePairs [] = []
mergePairs [xs] = [xs]
mergePairs (xs:ys:xss) = merge xs ys : mergePairs xss

--sort :: (Ord a)=>[a]->([a], Int)
sort = sort' . map (\x->([x], 0))

test = sort [5, 9, 1, 0, 5, 4, 3, 1, 2, 3]

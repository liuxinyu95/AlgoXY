-- QuickSort.hs, Quick Sort in Haskell
-- Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
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


import Test.QuickCheck
import qualified Data.List as L -- only for verification

-- Base version

bsort :: (Ord a) => [a] -> [a]
bsort [] = []
bsort (x:xs) = bsort [y | y<-xs, y<=x] ++ [x] ++ bsort [z | z<-xs, z>x]

-- Explicit partition
bsort' [] = []
bsort' (x:xs) = bsort' as ++ [x] ++ bsort' bs where
  (as, bs) = partition (<= x) xs
  
partition _ [] = ([], [])
partition p (x:xs) = let (as, bs) = partition p xs in
    if p x then (x:as, bs) else (as, x:bs)
                                
bsort'' [] = []                                
bsort'' (x:xs) = bsort'' as ++ [x] ++ bsort'' bs where
  (as, bs) = foldr f ([], []) xs
  f a (as', bs') = if a <= x then (a:as', bs') else (as', a:bs')

-- Accumulator version (Unstable version)
qsort :: (Ord a) => [a] -> [a]
qsort xs = qsort' xs []

qsort' [] r = r
qsort' [x] r = x:r
qsort' (x:xs) r = qpart xs [] [] r where
    qpart [] as bs r = qsort' as (x:qsort' bs r)
    qpart (x':xs') as bs r | x' <= x = qpart xs' (x':as) bs r
                           | x' >  x = qpart xs' as (x':bs) r
                                       
-- Explicit partition
asort xs = asort' xs []
  
asort' [] acc = acc
asort' (x:xs) acc = asort' as (x:asort' bs acc) where
  (as, bs) = part xs [] []
  part [] as bs = (as, bs)
  part (y:ys) as bs | y <= x = part ys (y:as) bs
                    | otherwise = part ys as (y:bs)

-- Ternery Quick Sort 

-- Base version
tsort :: (Ord a) => [a] -> [a]
tsort [] = []
tsort (x:xs) = tsort [a | a<-xs, a<x] ++ x:[b | b<-xs, b==x] ++ tsort [c | c<-xs, c>x]

-- Induct version. From Richard, Bird. ``Pearls of functional programming'', 
-- Chapter 12, Runking suffixes, section `a better rank', pp. 85

psort :: (Ord a) => [a] -> [a]
psort xs = concat $ pass xs []

pass [] xss = xss
pass (x:xs) xss = step xs [] [x] [] xss where
    step [] as bs cs xss = pass as (bs:pass cs xss)
    step (x':xs') as bs cs xss | x' <  x = step xs' (x':as) bs cs xss
                               | x' == x = step xs' as (x':bs) cs xss
                               | x' >  x = step xs' as bs (x':cs) xss

-- Changed according to quick sort accumulator version
tqsort :: (Ord a) => [a] -> [a]
tqsort xs = tqsort' xs []

tqsort' []     r = r
tqsort' (x:xs) r = qpart xs [] [x] [] r where
    qpart [] as bs cs r = tqsort' as (bs ++ tqsort' cs r)
    qpart (x':xs') as bs cs r | x' <  x = qpart xs' (x':as) bs cs r
                              | x' == x = qpart xs' as (x':bs) cs r
                              | x' >  x = qpart xs' as bs (x':cs) r

-- test

prop_bsort :: (Ord a, Num a) => [a] -> Bool
prop_bsort xs = L.sort xs == bsort xs

prop_bsort' :: [Int] -> Bool
prop_bsort' xs = L.sort xs == bsort' xs

prop_bsort'' :: [Int] -> Bool
prop_bsort'' xs = L.sort xs == bsort'' xs

prop_qsort :: (Ord a, Num a) => [a] -> Bool
prop_qsort xs = L.sort xs == qsort xs

prop_asort :: [Int] -> Bool
prop_asort xs = L.sort xs == asort xs

prop_tsort :: (Ord a, Num a) => [a] -> Bool
prop_tsort xs = L.sort xs == tsort xs

prop_psort :: (Ord a, Num a) => [a] -> Bool
prop_psort xs = L.sort xs == psort xs

prop_tqsort :: (Ord a, Num a) => [a] -> Bool
prop_tqsort xs = L.sort xs == tqsort xs


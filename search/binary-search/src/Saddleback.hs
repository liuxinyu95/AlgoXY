-- Saddleback.hs
-- Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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

-- Saddleback search based on Chapter 3 of [1]
-- [1] Richard Bird. ``Pearls of functional algorithm design''. Cambridge University Press. 2010. ISBN, 1139490605, 9781139490603

module Saddleback where

import Test.QuickCheck
import Data.List (sort)

-- brute-force naive search
-- In order to test easily, the brute force search actually starts from top-left corner, so that
--   It finds the solution in the same order as saddleback search.
bruteSolve f z = [(x, y) | x <- [0..z], y<- [z, z-1..0], f x y == z]

-- Saddleback basic version based on [2]
-- [2] Edsger W. Dijkstra. ``The saddleback search''. EWD-934. 1985. http://www.cs.utexas.edu/users/EWD/index09xx.html.
solve f z = search 0 z where
  search p q | p > z || q < 0 = []
             | z' < z = search (p + 1) q
             | z' > z = search p (q - 1)
             | otherwise = (p, q) : search (p + 1) (q - 1)
    where z' = f p q
                           
-- Minor improvement by using binary search to find the more accurate boundaries
-- Binary search in range (l, u)
bsearch f y (l, u) | u <= l = l
                   | f m <= y = if f (m + 1) <= y then bsearch f y (m + 1, u) else m
                   | otherwise = bsearch f y (l, m-1)
  where m = (l + u) `div` 2
        
solve' f z = search 0 m where
  search p q | p > n || q < 0 = []
             | z' < z = search (p + 1) q
             | z' > z = search p (q - 1)
             | otherwise = (p, q) : search (p + 1) (q - 1)
    where z' = f p q
  m = bsearch (f 0) z (0, z)
  n = bsearch (\x->f x 0) z (0, z)

-- Improvement based on [1]
solve'' f z = search f z (0, m) (n, 0) where
  m = bsearch (f 0) z (0, z)
  n = bsearch (\x -> f x 0) z (0, z)
  
search f z (a, b) (c, d) | c < a || b < d = []
                         | c - a < b - d = let q = (b + d) `div` 2 in csearch (bsearch (\x -> f x q) z (a, c), q)
                         | otherwise = let p = (a + c) `div` 2 in rsearch (p, bsearch (f p) z (d, b))
  where  
    csearch (p, q) | z < f p q = search f z (p, q - 1) (c, d)
                   | f p q == z = search f z (a, b) (p - 1, q + 1) ++ (p, q) : search f z (p + 1, q - 1) (c, d)
                   | otherwise = search f z (a, b) (p, q + 1) ++ search f z (p + 1, q - 1) (c, d)
    rsearch (p, q) | z < f p q = search f z (a, b) (p - 1, q)
                   | f p q == z = search f z (a, b) (p - 1, q + 1) ++ (p, q) : search f z (p + 1, q - 1) (c, d)
                   | otherwise = search f z (a, b) (p - 1, q + 1) ++ search f z (p + 1, q) (c, d)

-- test
fs = [\x y -> x + y, \x y -> 2^x + 3^y, \x y -> x^2 + y^2]

prop_solve :: Integer -> Bool
prop_solve z = let z' = abs z `mod` 100 in and $ map (\f -> solve f z' == bruteSolve f z') fs

bruteSearch f y (l, u) = if y < f l then l else head [ x | x <-[l..u], x == u || (f x <= y && y < f (x+1))]

testBSearch f y (l, u) = bruteSearch f y (l, u) == bsearch f y (l, u)

prop_bsearch :: Integer -> Integer -> Integer -> Bool
prop_bsearch y l u = and $ map (\f -> testBSearch (f 0) y' (min l' u', max l' u')) fs where
  [y', l', u'] = map abs [y, l, u]

prop_solve' :: Integer -> Bool
prop_solve' z = let z' = abs z `mod` 100 in and $ map (\f -> solve' f z' == solve f z') fs

prop_solve'' :: Integer -> Bool
prop_solve'' z = let z' = abs z `mod` 100 in and $ map (\f -> solve'' f z' == solve' f z') fs
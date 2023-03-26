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
import Test.QuickCheck -- for verification

atR :: [a] -> Int -> a
atR xs i = get xs (drop i xs) where
  get (x:_) [_] = x
  get (_:xs) (_:ys) = get xs ys

-- lastAt with zip
lastAt k xs = (fst . last) $ zip xs (drop k xs)

insertAt :: [a] -> Int -> a -> [a]
insertAt xs 0 y = y:xs
insertAt [] i y = [y]
insertAt (x:xs) i y = x : insertAt xs (i-1) y

permutation xs n r=  if length xs  <=  n - r
        then [[]]
        else [x:ys | x  <-  xs, ys  <-  permutation (delete x xs) n r]

-- Given a list of n elements, pick r from it to for permutation.
perm xs r | r == 0 || length xs < r = [[]]
          | otherwise = [ x:ys | x <-xs, ys <- perm (delete x xs) (r-1)]

-- Given a list of n elements, pick r from it to form combination.
comb _  0 = []
comb xs 1 = [[x] | x <- xs]
comb xs@(x:xs') r | length xs < r = []
                  | otherwise = [x:ys | ys <- comb xs' (r-1)] ++ comb xs' r

-- speakInt n = map speak (reverse $ zip ds ["", "thousand", "million", "billion"]) where
--   ds = reverse (digits n)
--   words = ["", "one", "two", "three", "four"]
--   speak (n, unit) | n < 20

digits = dec [] where
  dec ds n | n == 0 = ds
           | otherwise = dec ((n `mod` 1000) : ds) (n `div` 1000)

safeTake n [] = []
safeTake n (x:xs) | n <= 0 = []
                  | otherwise = x : safeTake (n - 1) xs

safeDrop n [] = []
safeDrop n (x:xs) | n <= 0 = x:xs
                  | otherwise = drop (n - 1) xs

sublist from cnt = take cnt . drop (from - 1)

slice from to = drop (from - 1) . take to

suffixes [] = [[]]
suffixes xs@(_:ys) =  xs : suffixes ys

sufs = foldr f [[]] where
  f x xss@(xs:_) = (x:xs) : xss

concats [] = []
concats ([]:xss) = concat xss
concats ((x:xs):xss) = x : concat (xs:xss)

foldr2 f z [] ys = z
foldr2 f z xs [] = z
foldr2 f z (x:xs) (y:ys) = foldr2 f (f x y z) xs ys

zip2 = foldr2 f [] where
  f x y xys = (x, y) : xys

iota1 = iota2 1

iota2 m n = iota' [] n where
  iota' ns n | n < m = ns
             | otherwise = iota' (n : ns) (n - 1)

iota m n a | m <= n = m : iota (m + a) n a
           | otherwise = []

iota4 m = m : iota4 (m + 1)

dedup  = foldr f [] where
  f x xs = x : filter (x /=) xs

-- test

prop_rindex :: [Int] -> Bool
prop_rindex xs = xs == (map (atR xs) $ reverse [0..length xs -1])

prop_insertAt :: [Int] -> Int -> Int -> Property
prop_insertAt xs i x = (0 <= i) ==> (insertAt xs i x) == (let (as, bs) = splitAt i xs in as ++ x:bs)

prop_concat :: [[Int]] -> Bool
prop_concat xss = concat xss == concats xss

prop_tails :: [Int] -> Bool
prop_tails xs = suffixes xs == xss && sufs xs == xss where
  xss = tails xs

prop_iota1 :: Int -> Bool
prop_iota1 z = let n = abs z in iota1 n == (take n $ iterate (+1) 1)

prop_iota2 :: Int -> Int -> Bool
prop_iota2 x y = iota2 m n == (take (n - m + 1) $ iterate (+1) m) where
  m = min (abs x) (abs y)
  n = max (abs x) (abs y)

prop_iota3 :: Int -> Int -> Bool
prop_iota3 x y = all (\a -> iota m n a == (takeWhile (<= n) $ iterate (+a) m)) [1..(n - m)]
  where
    m = min (abs x) (abs y)
    n = max (abs x) (abs y)

prop_iota4 :: Int -> Bool
prop_iota4 z = let m = abs z in take 10 (iota4 m) == (take 10 $ iterate (+1) m)

prop_nub :: [Int] -> Bool
prop_nub xs = nub xs == dedup xs

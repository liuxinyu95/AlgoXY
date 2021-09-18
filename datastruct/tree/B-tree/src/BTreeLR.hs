-- BTreeLR.hs
-- Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module BTreeLR where

-- Implement B-Tree with paired back-to-back lists.

import qualified Data.List as L
import Test.QuickCheck

data BTree a = Empty
             | BTree [(a, BTree a)] (BTree a) [(a, BTree a)]
             deriving (Eq, Show)

-- Define BTree of degree d as (d, t) :: (Int, BTree a) where
--   d - 1 <= |keys t| <= 2 * d - 1

full _ Empty = False
full d (BTree l _ r) = (length l + length r) > 2 * d - 1

low _ Empty = False
low d (BTree l _ r) = (length l + length r) < d - 1

stepL (BTree ((k, t):l) t' r) = BTree l t ((k, t'):r)

stepR (BTree l t' ((k, t):r)) = BTree ((k, t'):l) t r

partition x t@(BTree [] tm r)
  | x < (fst $ head r) = ([], tm, r)
  | otherwise = partition x (stepR t)
partition x t@(BTree l tm [])
  | x > (fst $ head l) = (l, tm, [])
  | otherwise = partition x (stepL t)
partition x t@(BTree l tm r)
  | (fst $ head l) < x && x < (fst $ head r) = (l, tm, r)
  | x > (fst $ head r) = partition x (stepR t)
  | x < (fst $ head l) = partition x (stepL t)

split :: Int -> (BTree a) -> ((BTree a), a, (BTree a))
split d t@(BTree l _ _) | n < d = sp $ iterate stepR t !! (d - n)
                        | n > d = sp $ iterate stepL t !! (n - d)
                        | otherwise = sp t
  where
    n = length l
    sp (BTree l t ((k, t'):r)) = (BTree l t [], k, BTree [] t' r)

insert :: (Ord a) => a -> (Int, BTree a) -> (Int, BTree a)
insert x (d, t) = fixRoot (d, ins t) where
  ins Empty = BTree [] Empty [(x, Empty)]
  ins t = let (l, t', r) = partition x t in
    case t' of
      Empty -> balance d l Empty ((x, Empty):r)
      _     -> balance d l (ins t') r

fixRoot (d, t) | full d t = let (t1, k, t2) = split d t in
                   (d, BTree [] t1 [(k, t2)])
               | otherwise = (d, t)

balance :: Int -> [(a, BTree a)] -> BTree a -> [(a, BTree a)] -> BTree a
balance d l t r | full d t = fixFull
                | otherwise = BTree l t r
  where
    fixFull = let (t1, k, t2) = split d t in BTree l t1 ((k, t2):r)

fromList d xs = foldr insert (d, Empty) xs

toList Empty = []
toList (BTree [] t r) = concat ((toList t) : map (\(k, tr) -> k : toList tr) r)
toList t = toList (stepL t)

-- Quick check
degOf [] = 2
degOf (x:_) = x `mod` 5 + 2 -- limit degree: 2 <= d < 6 for simplicity

isBTree :: Int -> Int -> (BTree a) -> Bool
isBTree _ _ Empty = True
isBTree d n t@(BTree l t' r) = (not $ full d t)
                          && (n == 0 || (not $ low d t))
                          && (and [isBTree d (n + 1) tr | (_, tr) <- (l ++ r)])

prop_order :: [Int] -> Bool
prop_order xs = (L.sort ys) == (toList $ snd $ fromList d ys) where
  d = degOf xs
  ys = L.nub xs

prop_insert :: [Int] -> Bool
prop_insert xs = isBTree d 0 $ snd $ fromList d ys where
  d = degOf xs
  ys = L.nub xs

testAll = do
  quickCheck prop_order
  quickCheck prop_insert

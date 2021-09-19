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

import Data.List (nub, sort)
import Test.QuickCheck
import Data.Maybe (listToMaybe)

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

hd = fst . head

partition p t@(BTree [] m r)
  | p (hd r) = partition p (stepR t)
  | otherwise = ([], m, r)
partition p t@(BTree l m [])
  | (not . p) (hd l) = partition p (stepL t)
  | otherwise = (l, m, [])
partition p t@(BTree l m r)
  | p (hd l) && (not . p) (hd r) = (l, m, r)
  | p (hd r) = partition p (stepR t)
  | (not . p) (hd l) = partition p (stepL t)

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
  ins t = let (l, t', r) = partition (< x) t in
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

lookup x Empty = Nothing
lookup x t = let (l, t', r) = partition (< x) t in
  if (Just x) == fmap fst (listToMaybe r) then Just (BTree l t' r)
  else BTreeLR.lookup x t'

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
                          && isBTree d (n + 1) t'
                          && (and [isBTree d (n + 1) tr | (_, tr) <- (l ++ r)])

prop_order :: [Int] -> Bool
prop_order xs = (sort ys) == (toList $ snd $ fromList d ys) where
  d = degOf xs
  ys = nub xs

prop_insert :: [Int] -> Bool
prop_insert xs = isBTree d 0 $ snd $ fromList d ys where
  d = degOf xs
  ys = nub xs

prop_lookup :: [Int] -> Int -> Bool
prop_lookup xs x = f $ BTreeLR.lookup x $ snd $ fromList d ys where
  d = degOf xs
  ys = nub xs
  f Nothing = not $ elem x xs
  f (Just (BTree l t [])) = False
  f (Just (BTree l t ((y, _):_))) = x == y

testAll = do
  quickCheck prop_order
  quickCheck prop_insert
  quickCheck prop_lookup

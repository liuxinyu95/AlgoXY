-- BTree.hs
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

module BTree where

-- Implement the B-Tree with List,
--   2-passes insert/delete (insert/delete then fix)

import qualified Data.List as L
import Test.QuickCheck
import Data.Maybe (listToMaybe)

data BTree a = BTree { keys :: [a]
                     , subTrees :: [BTree a] } deriving (Eq, Show)

empty = BTree [] []

-- Define BTree of degree d as (d, t) :: (Int, BTree a) where
--   d - 1 <= |keys t| <= 2 * d - 1

full d (BTree ks _) = (length ks) > 2 * d - 1

low d (BTree ks _) = (length ks) < d - 1

partition x (BTree ks ts) = (l, t, r) where
  l = (ks1, ts1)
  r = (ks2, ts2)
  (ks1, ks2) = L.span (< x) ks
  (ts1, (t:ts2)) = L.splitAt (length ks1) ts

split d (BTree ks ts) = (BTree ks1 ts1, k, BTree ks2 ts2) where
  (ks1, k:ks2) = L.splitAt (d - 1) ks
  (ts1, ts2) = L.splitAt d ts

unsplit (BTree ks1 ts1) k (BTree ks2 ts2) = BTree (ks1 ++ [k] ++ ks2)
                                                  (ts1 ++ ts2)

max' (BTree ks []) = last ks
max' (BTree _  ts) = max' $ last ts

insert :: (Ord a) => a -> (Int, BTree a) -> (Int, BTree a)
insert x (d, t) = fixRoot (d, ins t) where
    ins t@(BTree ks ts)
      | x `elem` ks = t -- skip duplicated key
      | null ts = BTree (L.insert x ks) []
      | otherwise = balance d l (ins t') r where
          (l, t', r) = partition x t

delete :: (Ord a, Eq a) => a -> (Int, BTree a) -> (Int, BTree a)
delete x (d, t) = fixRoot (d, del x t) where
    del x (BTree ks []) = BTree (L.delete x ks) []
    del x t = if (Just x) == listToMaybe ks' then
                let k' = max' t' in balance d l (del k' t') (k':(tail ks'), ts')
              else balance d l (del x t') r
      where
        (l, t', r@(ks', ts')) = partition x t

fixRoot (d, BTree [] [t]) = (d, t)
fixRoot (d, t) | full d t  = let (t1, k, t2) = split d t in
                               (d, BTree [k] [t1, t2])
               | otherwise = (d, t)

balance :: Int -> ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
balance d (ks1, ts1) t (ks2, ts2)
    | full d t  = fixFull
    | low  d t  = fixLow
    | otherwise = BTree (ks1 ++ ks2) (ts1 ++ [t] ++ ts2)
  where
    fixFull = let (t1, k, t2) = split d t in
                BTree (ks1 ++ [k] ++ ks2) (ts1 ++ [t1, t2] ++ ts2)
    fixLow | not $ null ks1 = balance d (init ks1, init ts1)
                                      (unsplit (last ts1) (last ks1) t)
                                      (ks2, ts2)
           | not $ null ks2 = balance d (ks1, ts1)
                                      (unsplit t (head ks2) (head ts2))
                                      (tail ks2, tail ts2)
           | otherwise = t -- ks2 == ks1 == []

lookup k t@(BTree ks []) = if k `elem` ks then Just t else Nothing
lookup k t = if (Just k) == listToMaybe ks then Just t
             else BTree.lookup k t'  where
  (_, t', (ks, _)) = partition k t

fromList d xs = foldr insert (d, empty) xs

toList (BTree ks []) = ks
toList (BTree ks (t:ts)) = concat ((toList t) : (zipWith (\k tr -> k : toList tr) ks ts))

foldt :: (a -> b -> b) -> b -> (BTree a) -> b
foldt f z (BTree ks []) = foldr f z ks
foldt f z (BTree [] [t]) = foldt f z t
foldt f z (BTree (k:ks) (t:ts)) = let z' = foldt f z (BTree ks ts) in
  foldt f (f k z') t

-- Quick check

degOf [] = 2
degOf (x:_) = x `mod` 5 + 2 -- limit degree: 2 <= d < 6 for simplicity

isBTree :: Int -> Int -> (BTree a) -> Bool
isBTree d n t@(BTree _ ts) = (not $ full d t)
                          && (n == 0 || (not $ low d t))
                          && (and [isBTree d (n + 1) tr | tr <- ts])

prop_order :: [Int] -> Bool
prop_order xs = (L.sort $ L.nub xs) == (toList $ snd $ fromList d xs) where
  d = degOf xs

prop_insert :: [Int] -> Bool
prop_insert xs = isBTree d 0 $ snd $ fromList d xs where d = degOf xs

prop_delete :: [Int] -> Int -> Bool
prop_delete xs x = (L.sort $ L.delete x ys) ==
                   (toList $ snd $ delete x $ fromList d ys) where
  ys = L.nub xs
  d = degOf xs

prop_del_balance :: [Int] -> Int -> Bool
prop_del_balance xs x = isBTree d 0 $ snd $ delete x $ fromList d ys where
  ys = L.nub xs
  d = degOf xs

prop_lookup :: [Int] -> Int -> Bool
prop_lookup xs x = f $ BTree.lookup x $ snd $ fromList d xs where
  d = degOf xs
  f Nothing = not $ elem x xs
  f (Just (BTree ks _)) = x `elem` ks

prop_foldt :: [Int] -> Bool
prop_foldt xs = toList t == foldt (:) [] t where
  t = snd $ fromList d xs
  d = degOf xs

testAll = do
  quickCheck prop_order
  quickCheck prop_insert
  quickCheck prop_delete
  quickCheck prop_del_balance
  quickCheck prop_lookup
  quickCheck prop_foldt

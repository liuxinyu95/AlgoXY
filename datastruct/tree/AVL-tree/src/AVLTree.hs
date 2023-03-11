-- AVLTree.hs
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

module AVLTree where

import Test.QuickCheck
import qualified Data.List as L -- for verification purpose only
import Prelude hiding (min)

data AVLTree a = Empty
               | Br (AVLTree a) a (AVLTree a) Int

isEmpty Empty = True
isEmpty _ = False

min (Br Empty x _ _) = x
min (Br l _ _ _) = min l

insert x  = fst . ins x where
    -- result of ins is a pair (t, d), t: tree, d: increment of height
    ins x Empty = (Br Empty x Empty 0, 1)
    ins x (Br l k r d)
        | x < k     = tree (ins x l) k (r, 0) d
        | x == k    = (Br l k r d, 0)
        | otherwise = tree (l, 0) k (ins x r) d

delete :: (Ord a) => a -> AVLTree a -> AVLTree a
delete x = fst . del x where
  -- result is a pair (t, d), t: tree, d: decrement in height
  del _ Empty = (Empty, 0)
  del x (Br l k r d)
    | x < k = tree (del x l) k (r, 0) d
    | x > k = tree (l, 0) k (del x r) d
    -- x == k, delete this node
    | isEmpty l = (r, -1)
    | isEmpty r = (l, -1)
    | otherwise = tree (l, 0) m (del m r) d where m = min r

-- params: (left, increment on left) key (right, increment on right)
tree (l, dl) k (r, dr) d = balance (Br l k r d') deltaH where
    d' = d + dr - dl
    deltaH | d >=0 && d' >=0 = dr
           | d <=0 && d' >=0 = d+dr
           | d >=0 && d' <=0 = dl - d
           | otherwise = dl

-- balance :: AVLTree a -> Int -> (AVLTree a, Int)
balance (Br (Br (Br a x b dx) y c (-1)) z d (-2)) dH = (Br (Br a x b dx) y (Br c z d 0) 0, dH-1)
balance (Br a x (Br b y (Br c z d dz)    1)    2) dH = (Br (Br a x b 0) y (Br c z d dz) 0, dH-1)
balance (Br (Br a x (Br b y c dy)    1) z d (-2)) dH = (Br (Br a x b dx') y (Br c z d dz') 0, dH-1) where
    dx' = if dy ==  1 then -1 else 0
    dz' = if dy == -1 then  1 else 0
balance (Br a x (Br (Br b y c dy) z d (-1))    2) dH = (Br (Br a x b dx') y (Br c z d dz') 0, dH-1) where
    dx' = if dy ==  1 then -1 else 0
    dz' = if dy == -1 then  1 else 0
-- Delete specific fixing
balance (Br (Br a x b dx) y c (-2)) dH = (Br a x (Br b y c (-1)) (dx+1), dH)
balance (Br a x (Br b y c dy)    2) dH = (Br (Br a x b    1) y c (dy-1), dH)
balance t d = (t, d)

-- validate AVL tree
isAVL :: (AVLTree a) -> Bool
isAVL Empty = True
isAVL (Br l _ r d) = and [isAVL l, isAVL r, d == (height r - height l), abs d <= 1]

height :: (AVLTree a) -> Int
height Empty = 0
height (Br l _ r _) = 1 + max (height l) (height r)

checkDelta :: (AVLTree a) -> Bool
checkDelta Empty = True
checkDelta (Br l _ r d) = and [checkDelta l, checkDelta r, d == (height r - height l)]

fromList :: (Ord a) => [a] -> AVLTree a
fromList = foldr insert Empty

toList :: (AVLTree a) -> [a]
toList Empty = []
toList (Br l k r _) = toList l ++ [k] ++ toList r

-- test
prop_bst :: (Ord a, Num a) => [a] -> Bool
prop_bst xs = (L.sort $ L.nub xs) == (toList $ fromList xs)

prop_delta :: (Ord a, Num a) => [a] -> Bool
prop_delta = checkDelta . fromList. L.nub

prop_avl :: (Ord a, Num a) => [a] -> Bool
prop_avl = isAVL . fromList . L.nub

prop_insert :: (Ord a, Num a) => [a] -> Bool
prop_insert = snd . (foldl verifyInsert (Empty, True)) . L.nub where
  verifyInsert (_, False) _ = (Empty, False)
  verifyInsert (t, _) x = let t' = insert x t in (t', isAVL t')

prop_del :: (Ord a, Num a) => [a] -> Bool
prop_del = verifyDel . L.nub where
  verifyDel [] = True
  verifyDel xs@(x:xs') = (toList $ delete x (fromList xs)) == L.sort xs'

prop_del_avl :: (Ord a, Num a) => [a] -> Bool
prop_del_avl = verifyDelAVL . L.nub where
  verifyDelAVL [] = True
  verifyDelAVL xs@(x:_) = isAVL $ delete x (fromList xs)

prop_delete :: (Ord a, Num a) => [a] -> Bool
prop_delete xs = snd $ foldl verifyDel (tr, isAVL tr) xs' where
  xs' = L.nub xs
  tr = fromList xs'
  verifyDel (_, False) _ = (Empty, False)
  verifyDel (t, _) x = let t' = delete x t in (t', isAVL t')

testAll = do
  quickCheck (prop_bst::[Int]->Bool)
  quickCheck (prop_delta::[Int]->Bool)
  quickCheck (prop_avl::[Int]->Bool)
  quickCheck (prop_insert::[Int]->Bool)
  quickCheck (prop_del::[Int]->Bool)
  quickCheck (prop_del_avl::[Int]->Bool)
  quickCheck (prop_delete::[Int]->Bool)

instance Show a => Show (AVLTree a) where
    show Empty = "."
    show (Br l k r d) = "(" ++ show l ++ " " ++
                          show k ++ ":[" ++ show d ++ "] " ++
                          show r ++ ")"

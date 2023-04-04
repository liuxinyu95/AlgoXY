-- RBTree.hs, Haskell Red-Black tree implementation
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


-- Mark a node 'deleted' without actual removing it, rebuild the tree when such
-- nodes exceed 50%.

module RBTreeEx where

import Data.Function (on)
import Test.QuickCheck
import qualified Data.List as L

data Color = R | B deriving (Show, Eq)
data RBTree a = Empty
              | Node Color (RBTree a) a (RBTree a)

data Elem a = Elem a Bool Int Int deriving (Eq)

active (Elem _ a _ _) = a
getElem (Elem x _ _ _) = x

instance Ord a => Ord (Elem a) where
  compare = compare `on` getElem

insert x = makeBlack . ins (Elem x True 1 1) where
    ins e Empty = Node R Empty e Empty
    ins e (Node color l k r)
        | e < k     = balance color (ins e l) k r
        | otherwise = balance color l k (ins e r)
    makeBlack (Node _ l k r) = Node B l k r

balance B (Node R (Node R a x b) y c) z d = node R (node B a x b) y (node B c z d)
balance B (Node R a x (Node R b y c)) z d = node R (node B a x b) y (node B c z d)
balance B a x (Node R b y (Node R c z d)) = node R (node B a x b) y (node B c z d)
balance B a x (Node R (Node R b y c) z d) = node R (node B a x b) y (node B c z d)
balance color l k r = node color l k r

node c l (Elem k a _ _) r = Node c l (Elem k a sz ca) r
  where
    sz = size l + size r + if a then 1 else 0
    ca = cap l + cap r + 1

size Empty = 0
size (Node _ _ (Elem _ _ sz _) _) = sz

cap Empty = 0
cap (Node _ _ (Elem _ _ _ ca) _) = ca

delete x = rebuild . del x where
  del _ Empty = Empty
  del x (Node c l e@(Elem k a sz ca) r)
    | x < k = node c (del x l) e r
    | x > k = node c l e (del x r)
    | x == k = node c l (Elem k False 0 0) r

rebuild t | 2 * size t < cap t = (fromList . toList) t
          | otherwise = t

fromList :: (Ord a) => [a] -> RBTree (Elem a)
fromList = foldr insert Empty

trSort :: Ord a => [a] -> [a]
trSort = toList . fromList

toList Empty = []
toList (Node _ l e r) | active e = toList l ++ [getElem e] ++ toList r
                      | otherwise = toList l ++ toList r

prop_bst :: [Int] -> Bool
prop_bst xs = let ys = L.nub xs in L.sort ys == (trSort ys)

prop_del :: [Int] -> Int -> Bool
prop_del xs x = let ys = L.nub xs in
  L.sort (L.delete x ys) == (toList $ delete x $ fromList ys)

prop_rebuild :: [Int] -> Bool
prop_rebuild xs = L.sort bs == (toList $ foldr delete (fromList ys) as)
  where
    ys = L.nub xs
    n = 1 + (length ys) `div` 2
    (as, bs) = splitAt n ys

testAll = do
  quickCheck prop_bst
  quickCheck prop_del

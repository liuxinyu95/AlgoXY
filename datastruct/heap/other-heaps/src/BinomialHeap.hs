{-
    BinomialHeap.hs, Binomial Heap in Haskell
    Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

-- Based on Chris Okasaki's ``Purely Functional Data structures''

module BinomialHeap where

import Test.QuickCheck
import Data.Function (on)
import qualified Data.List as L -- for verification purpose only

data BiTree a = Node { rank :: Int
                     , key :: a
                     , subTrees :: [BiTree a]} deriving (Eq, Show)

type BiHeap a = [BiTree a]  -- ascending by rank

-- Link two trees with SAME rank R to a new tree of rank R+1
link t1@(Node r x c1) t2@(Node _ y c2) =
    if x < y then Node (r+1) x (t2:c1)
    else Node (r+1) y (t1:c2)

insertTree t [] = [t]
insertTree t ts@(t':ts') | rank t < rank t' = t:ts
                         | rank t > rank t' = t' : insertTree t ts'
                         | otherwise = insertTree (link t t') ts'

insert x = insertTree (Node 0 x [])

merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1:(merge ts1' ts2)
    | rank t1 > rank t2 = t2:(merge ts1 ts2')
--    | otherwise = insertTree (link t1 t2) (merge ts1' ts2')
    | otherwise = merge (insertTree (link t1 t2) ts1') ts2'

top :: (Ord a) => BiHeap a -> a
top = key . (L.minimumBy (compare `on` key))

pop h = merge (reverse $ subTrees t) ts where
    (t, ts) = extractMin h

extractMin [t] = (t, [])
extractMin (t:ts) = if key t < key t' then (t, ts)
                    else (t', t:ts') where
    (t', ts') = extractMin ts

-- Helper functions

fromList :: (Ord a) => [a] -> BiHeap a
fromList = foldr insert []

heapSort :: (Ord a) => [a] -> [a]
heapSort = hsort . fromList where
    hsort [] = []
    hsort h = (top h):(hsort $ pop h)

-- test

prop_sort :: [Int] -> Bool
prop_sort xs = heapSort xs == L.sort xs

prop_binomial :: [Int] -> Bool
prop_binomial = isBinomial . fromList where
    isBinomial [] = True
    isBinomial xs = all isBinomialT xs

isBinomialT (Node 0 _ []) = True
isBinomialT (Node r x cs) = all isBinomialT [t1, t2] && r == 1 + rank t2
    where
      t1 = head cs
      t2 = Node (r-1) x (tail cs)

testAll = do
  quickCheck prop_sort
  quickCheck prop_binomial

{-
    FibonacciHeap.hs, Binomial Heap in Haskell
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

-- Fibonacci heap is a kind of lazy Binomial heap.

module FibonacciHeap where

import Test.QuickCheck
import qualified Data.List as L -- for verification purpose only.

data BiTree a = Node { rank :: Int
                     , key :: a
                     , subTrees :: [BiTree a]} deriving (Eq, Show)

data FibHeap a = E | FH { size :: Int
                        , minTree :: BiTree a
                        , trees :: [BiTree a]} deriving (Eq, Show)

singleton x = FH 1 (Node 1 x []) []

link :: (Ord a) => BiTree a -> BiTree a -> BiTree a
link t1@(Node r x c1) t2@(Node _ y c2)
    | x<y = Node (r+1) x (t2:c1)
    | otherwise = Node (r+1) y (t1:c2)

insert :: (Ord a) => a -> FibHeap a -> FibHeap a
insert = merge . singleton  -- O(1)

-- ++ is O(\lg n1) or O(\g n2) time.
merge h E = h
merge E h = h
merge h1@(FH sz1 minTr1 ts1) h2@(FH sz2 minTr2 ts2)
    | key minTr1 < key minTr2 = FH (sz1 + sz2) minTr1 (minTr2 : ts2 ++ ts1)
    | otherwise = FH (sz1 + sz2) minTr2 (minTr1 : ts1 ++ ts2)

-- Find Minimum element in O(1) time

findMin :: (Ord a) => FibHeap a -> a
findMin = key . minTree

-- deleting, Amortized O(lg N) time

-- Auxiliary function

-- Consolidate unordered Binomial trees by melding all trees in same rank
--  O(lg N) time

consolidate :: (Ord a) => [BiTree a] -> [BiTree a]
consolidate = foldl meld [] where
    meld [] t = [t]
    meld (t':ts) t | rank t == rank t' = meld ts (link t t')
                   | rank t <  rank t' = t:t':ts
                   | otherwise = t' : meld ts t

-- Find the tree which contains the minimum element.
-- Returns the minimum element tree and the left trees as a pair
--   O(lg N) time

extractMin :: (Ord a) => [BiTree a] -> (BiTree a, [BiTree a])
extractMin [t] = (t, [])
extractMin (t:ts) = if key t < key t' then (t, ts)
                        else (t', t:ts')
    where
      (t', ts') = extractMin ts

-- delete function

deleteMin :: (Ord a) => FibHeap a -> FibHeap a
deleteMin (FH _ (Node _ x []) []) = E
deleteMin h@(FH sz minTr ts) = FH (sz-1) minTr' ts' where
    (minTr', ts') = extractMin $ consolidate (subTrees minTr ++ ts)

-- Helper functions

-- This function performs badly because it actually create a linked-list
-- The ideal way is to insert and delete randomly, so that the amortized
-- performance dominate.
fromList :: (Ord a) => [a] -> FibHeap a
fromList = foldr insert E

-- This testing has the same problem with fromList, as it actually
-- first create a linked-list, then during deleteMin, it start merge
-- them to Binomial heap, the first consolidation takes very long time.
heapSort :: (Ord a) => [a] -> [a]
heapSort = hsort . fromList where
    hsort E = []
    hsort h = (findMin h):(hsort $ deleteMin h)

-- test

prop_sort :: [Int] -> Bool
prop_sort xs = heapSort xs == L.sort xs

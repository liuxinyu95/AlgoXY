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

import BinomialHeap (BiTree(..), link, extractMin)
import Test.QuickCheck
import qualified Data.List as L

data FibHeap a = E | FH { size :: Int
                        , minTree :: BiTree a
                        , trees :: [BiTree a]} deriving (Eq, Show)

singleton x = FH 1 (Node 1 x []) []

insert :: (Ord a) => a -> FibHeap a -> FibHeap a
insert = merge . singleton  -- O(1)

-- ++ is O(\lg n1) or O(\lg n2) time.
merge h E = h
merge E h = h
merge h1@(FH sz1 minTr1 ts1) h2@(FH sz2 minTr2 ts2)
    | key minTr1 < key minTr2 = FH (sz1 + sz2) minTr1 (minTr2 : ts2 ++ ts1)
    | otherwise = FH (sz1 + sz2) minTr2 (minTr1 : ts1 ++ ts2)

top :: (Ord a) => FibHeap a -> a
top = key . minTree

-- deleting, Amortized O(lg n) time

--  O(lg n) time
consolidate :: (Ord a) => [BiTree a] -> [BiTree a]
consolidate = foldr melt [] where
    melt t [] = [t]
    melt t (t':ts) | rank t == rank t' = melt (link t t') ts
                   | rank t <  rank t' = t : t' : ts
                   | otherwise = t' : melt t ts

pop (FH _ (Node _ x []) []) = (x, E)
pop (FH sz (Node _ x tsm) ts) = (x, FH (sz-1) tm ts') where
    (tm, ts') = extractMin $ consolidate (tsm ++ ts)

-- This function performs poor because it actually creates a
-- linked-list due to lazy consolidation. Ideally, insert/delete
-- happen randomly to secure the amortized performance
fromList :: (Ord a) => [a] -> FibHeap a
fromList = foldr insert E

-- Same call out as fromList, the first consolidation triggered
-- by pop restructures all nodes to a binomial heap.
heapSort :: (Ord a) => [a] -> [a]
heapSort = hsort . fromList where
    hsort E = []
    hsort h = x :(hsort h') where (x, h') = pop h

-- test
prop_sort :: [Int] -> Bool
prop_sort xs = heapSort xs == L.sort xs

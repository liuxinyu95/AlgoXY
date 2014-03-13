{-
    Heap.hs, Simple heap realized with pairing heap
    Copyright (C) 2014, Liu Xinyu (liuxinyu95@gmail.com)

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

-- Based on Chris Okasaki's ``Purely Functional Datastructures''

module Heap where

-- Definition

data PHeap a = E | Node a [PHeap a] deriving (Eq, Show)

-- Merge, O(1) time

merge :: (Ord a) => PHeap a -> PHeap a -> PHeap a
merge h E = h
merge E h = h
merge h1@(Node x hs1) h2@(Node y hs2) =
    if x < y then Node x (h2:hs1) else Node y (h1:hs2)

-- Insert, O(1) time

insert :: (Ord a) => PHeap a -> a -> PHeap a
insert h x = merge (Node x []) h

-- Find minimum element in O(1) time

findMin :: PHeap a -> a
findMin (Node x _) = x

-- delete the minimum element in O(lg N) amortized time (Conjecture)
deleteMin :: (Ord a) => PHeap a -> PHeap a
deleteMin (Node _ hs) = mergePairs hs where
    mergePairs [] = E
    mergePairs [h] = h
    mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

-- Helper functions

fromList :: (Ord a) => [a] -> PHeap a
fromList xs = foldl insert E xs


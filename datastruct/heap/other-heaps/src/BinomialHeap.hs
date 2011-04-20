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

-- Based on Chris Okasaki's ``Purely Functional Datastructures''

module BinomialHeap where

-- Definition

data BiTree a = Node { rank :: Int
                     , root :: a
                     , children :: [BiTree a]} deriving (Eq, Show)

-- Implicit property: ranks are in monotonically increase order
type BiHeap a = [BiTree a] 


-- Auxiliary functions


-- Link 2 trees with SAME rank R to a new tree of rank R+1
link :: (Ord a) => BiTree a -> BiTree a -> BiTree a
link t1@(Node r x c1) t2@(Node _ y c2) = 
    if x<y then Node (r+1) x (t2:c1)
    else Node (r+1) y (t1:c2)

-- Insert a Binomial tree into a Binomial heap
--  Implicit condition: the rank of tree is either lower or equal to the first
--  element tree in the heap.
insertTree :: (Ord a) => BiHeap a -> BiTree a -> BiHeap a
insertTree [] t = [t]
insertTree ts@(t':ts') t = if rank t < rank t' then t:ts
                           else insertTree ts' (link t t')

-- Insertion

insert :: (Ord a) => BiHeap a -> a -> BiHeap a
insert h x = insertTree h (Node 0 x [])

-- Merge

merge:: (Ord a) => BiHeap a -> BiHeap a -> BiHeap a
merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@(t1:ts1') ts2@(t2:ts2') 
    | rank t1 < rank t2 = t1:(merge ts1' ts2)
    | rank t1 > rank t2 = t2:(merge ts1 ts2')
    | otherwise = insertTree (merge ts1' ts2') (link t1 t2)


-- Auxiliary function for deleting

removeMinTree :: (Ord a) => BiHeap a -> (BiTree a, BiHeap a)
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if root t < root t' then (t, ts) 
                       else (t', t:ts')
    where
      (t', ts') = removeMinTree ts

findMin :: (Ord a) => BiHeap a -> a
findMin = root . fst. removeMinTree

deleteMin :: (Ord a) => BiHeap a -> BiHeap a
deleteMin h = merge (reverse $ children t) ts where
    (t, ts) = removeMinTree h

-- Helper functions

fromList :: (Ord a) => [a] -> BiHeap a
fromList xs = foldl insert [] xs

heapSort :: (Ord a) => [a] -> [a]
heapSort = hsort . fromList where
    hsort [] = []
    hsort h = (findMin h):(hsort $ deleteMin h)

-- test

testFromList = fromList [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]

testHeapSort = heapSort [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
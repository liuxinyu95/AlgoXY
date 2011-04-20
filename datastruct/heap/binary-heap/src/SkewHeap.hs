{-
    LeftistHeap.hs, Leftist Heap in Haskell
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

module SkewHeap where

-- Definition

data SHeap a = E -- Empty 
             | Node a (SHeap a) (SHeap a) -- element, left, right
               deriving (Eq, Show)

merge::(Ord a)=>SHeap a -> SHeap a -> SHeap a
merge E h = h
merge h E = h
merge h1@(Node x l r) h2@(Node y l' r') = 
    if x < y then Node x (merge r h2) l
    else Node y (merge h1 r') l'

insert::(Ord a)=> SHeap a -> a -> SHeap a
insert h x = merge (Node x E E) h

findMin :: SHeap a -> a
findMin (Node x _ _) = x

deleteMin :: (Ord a) => SHeap a -> SHeap a
deleteMin (Node _ l r) = merge l r

fromList :: (Ord a) => [a] -> SHeap a
fromList = foldl insert E

heapSort :: (Ord a) => [a] -> [a]
heapSort = hsort . fromList where
    hsort E = []
    hsort h = (findMin h):(hsort $ deleteMin h)

-- test

testFromList = fromList [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]

testHeapSort = heapSort [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]
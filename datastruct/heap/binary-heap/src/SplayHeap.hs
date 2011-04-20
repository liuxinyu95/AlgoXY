{-
    SplayHeap.hs, Splay Heap in Haskell
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

module SplayHeap where

import System.Random -- only for testing

-- Definition

data STree a = E -- Empty
             | Node (STree a) a (STree a) -- left, element, right
               deriving (Eq, Show)

-- partition the tree in two parts based on a pivot value, 
--  less part contains all elements < pivot
--  bigger part contains all elements >= pivot
partition :: (Ord a) => STree a -> a -> (STree a, STree a)
partition E _ = (E, E)
partition t@(Node l x r) y 
    | x < y = 
        case r of
          E -> (t, E)
          Node l' x' r' -> 
              if x' < y then 
                  let (small, big) = partition r' y in
                  (Node (Node l x l') x' small, big)
              else 
                  let (small, big) = partition l' y in
                  (Node l x small, Node big x' r')
    | otherwise = 
        case l of
          E -> (E, t)
          Node l' x' r' ->
              if y < x' then
                  let (small, big) = partition l' y in
                  (small, Node l' x' (Node r' x r))
              else
                  let (small, big) = partition r' y in
                  (Node l' x' small, Node big x r)
                    
insert :: (Ord a) => STree a -> a -> STree a
insert t x = Node small x big where (small, big) = partition t x

merge :: (Ord a) => STree a -> STree a -> STree a
merge E t = t
merge (Node l x r) t = Node (merge l l') x (merge r r')
    where (l', r') = partition t x

findMin :: STree a -> a
findMin (Node E x _) = x
findMin (Node l x _) = findMin l

deleteMin :: STree a -> STree a
deleteMin (Node E x r) = r
deleteMin (Node (Node E x' r') x r) = Node r' x r
deleteMin (Node (Node l' x' r') x r) = Node (deleteMin l') x' (Node r' x r)

-- splay by pattern matching
splay :: (Eq a) => STree a -> a ->STree a
-- zig-zig
splay t@(Node (Node (Node a x b) p c) g d) y =
    if x == y then Node a x (Node b p (Node c g d)) else t
splay t@(Node a g (Node b p (Node c x d))) y =
    if x == y then Node (Node (Node a g b) p c) x d else t
-- zig-zag
splay t@(Node (Node a p (Node b x c)) g d) y =
    if x == y then Node (Node a p b) x (Node c g d) else t
splay t@(Node a g (Node (Node b x c) p d)) y =
    if x == y then Node (Node a g b) x (Node c p d) else t
-- zig
splay t@(Node (Node a x b) p c) y = if x == y then Node a x (Node b p c) else t
splay t@(Node a p (Node b x c)) y = if x == y then Node (Node a p b) x c else t
-- otherwise
splay t _ = t

-- insert by using pattern matching
insert' :: (Ord a) => STree a -> a -> STree a
insert' E y = Node E y E
insert' (Node l x r) y 
    | x > y     = splay (Node (insert' l y) x r) y
    | otherwise = splay (Node l x (insert' r y)) y

lookup' :: (Ord a) => STree a -> a -> STree a
lookup' E _ = E
lookup' t@(Node l x r) y
    | x == y    = t
    | x > y     = splay (Node (lookup' l y) x r) y
    | otherwise = splay (Node l x (lookup' r y)) y

-- general part for heap

fromList :: (Ord a) => [a] -> STree a
fromList xs = foldl insert E xs

heapSort :: (Ord a) => [a] -> [a]
heapSort = hsort . fromList where
    hsort E = []
    hsort h = (findMin h):(hsort $ deleteMin h)

-- test

testFromList = fromList [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]

testHeapSort = heapSort [16, 14, 10, 8, 7, 9, 3, 2, 4, 1]

testFromList' = do
  putStrLn $ show $ tolst [16, 14, 10, 8, 7, 9, 3, 2, 4, 1] 
  putStrLn $ show $ tolst [16, 10, 14, 8, 1, 7, 9, 3, 4, 2]
           where
             tolst = foldl insert' E 


toStr :: (Show a)=>(STree a) -> String
toStr E = "."
toStr (Node l x r) = "(" ++ (toStr l) ++ " " ++ 
                     (show x) ++ " " ++ (toStr r) ++ ")"


--testSplay :: IO (STree Int)
testSplay = do
  xs <- sequence (replicate 1000 (randomRIO(1, 10)))
  putStrLn $ show (foldl lookup' t xs)
  putStrLn $ show $ toStr (foldl lookup' t xs) -- in-order display
      where 
        t = foldl insert' (E::STree Int) [1..10]
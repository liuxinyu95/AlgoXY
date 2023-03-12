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
partition :: (Ord a) => a -> STree a -> (STree a, STree a)
partition _ E = (E, E)
partition y t@(Node l x r)
    | x < y =
        case r of
          E -> (t, E)
          Node l' x' r' ->
              if x' < y then
                  let (small, big) = partition y r' in
                  (Node (Node l x l') x' small, big)
              else
                  let (small, big) = partition y l' in
                  (Node l x small, Node big x' r')
    | otherwise =
        case l of
          E -> (E, t)
          Node l' x' r' ->
              if y < x' then
                  let (small, big) = partition y l' in
                  (small, Node l' x' (Node r' x r))
              else
                  let (small, big) = partition y r' in
                  (Node l' x' small, Node big x r)

insert :: (Ord a) => a -> STree a -> STree a
insert x t = Node small x big where (small, big) = partition x t

merge :: (Ord a) => STree a -> STree a -> STree a
merge E t = t
merge (Node l x r) t = Node (merge l l') x (merge r r')
    where (l', r') = partition x t

findMin :: STree a -> a
findMin (Node E x _) = x
findMin (Node l x _) = findMin l

deleteMin :: STree a -> STree a
deleteMin (Node E x r) = r
deleteMin (Node (Node E x' r') x r) = Node r' x r
deleteMin (Node (Node l' x' r') x r) = Node (deleteMin l') x' (Node r' x r)

-- splay by pattern matching
splay :: (Eq a) => a -> STree a -> STree a
-- zig-zig
splay y t@(Node (Node (Node a x b) p c) g d) =
    if x == y then Node a x (Node b p (Node c g d)) else t
splay y t@(Node a g (Node b p (Node c x d))) =
    if x == y then Node (Node (Node a g b) p c) x d else t
-- zig-zag
splay y t@(Node (Node a p (Node b x c)) g d) =
    if x == y then Node (Node a p b) x (Node c g d) else t
splay y t@(Node a g (Node (Node b x c) p d)) =
    if x == y then Node (Node a g b) x (Node c p d) else t
-- zig
splay y t@(Node (Node a x b) p c) = if x == y then Node a x (Node b p c) else t
splay y t@(Node a p (Node b x c)) = if x == y then Node (Node a p b) x c else t
-- otherwise
splay _ t = t

-- insert by using pattern matching
insert' :: (Ord a) => a -> STree a -> STree a
insert' y E = Node E y E
insert' y (Node l x r)
    | x > y     = splay y (Node (insert' y l) x r)
    | otherwise = splay y (Node l x (insert' y r))

member :: (Ord a) => a -> STree a -> (Bool, STree a)
member _ E = (False, E)
member y t@(Node l x r)
    | x == y    = (True, t)
    | x > y     = let (e, l') = member y l in (e, splay y (Node l' x r))
    | otherwise = let (e, r') = member y r in (e, splay y (Node l x r'))

-- general part for heap

fromList :: (Ord a) => [a] -> STree a
fromList = foldr insert E

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
             tolst = foldr insert' E


toStr :: (Show a)=>(STree a) -> String
toStr E = "."
toStr (Node l x r) = "(" ++ (toStr l) ++ " " ++
                     (show x) ++ " " ++ (toStr r) ++ ")"


--testSplay :: IO (STree Int)
testSplay = do
  xs <- sequence (replicate 1000 (randomRIO(1, 10)))
  putStrLn $ show (foldr ((snd .) . member) t xs)
  putStrLn $ show $ toStr (foldr ((snd .) . member) t xs) -- in-order display
      where
        t = foldr insert' (E::STree Int) [1..10]

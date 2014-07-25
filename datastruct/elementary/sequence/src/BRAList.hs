{-
    BRAList.hs, Binary Random Access List without numeric representation.
    Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)

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

module BRAList where

import Test.QuickCheck

-- Based on Chris Okasaki's work in [1]

-- Binary tree representation

data Tree a = Leaf a
            | Node Int (Tree a) (Tree a) -- size, left, right
              deriving (Show)

-- The random access list is represented as a forest of binary trees.
type BRAList a = [Tree a]

-- Auxilary functions
size :: Tree a -> Int
size (Leaf _) = 1
size (Node sz _ _) = sz

-- Precondition: rank t1 = rank t2
link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

cons :: a -> BRAList a -> BRAList a
cons x ts = insertTree ts (Leaf x) 

insertTree :: BRAList a -> Tree a -> BRAList a
insertTree [] t = [t]
insertTree (t':ts) t = if size t < size t' then  t:t':ts
                       else insertTree ts (link t t')

-- Assert the BRAList isn't empty
extractTree :: BRAList a -> (Tree a, BRAList a)
extractTree (t@(Leaf x):ts) = (t, ts)
extractTree (t@(Node _ t1 t2):ts) = extractTree (t1:t2:ts)

head' :: BRAList a -> a
head' ts = x where (Leaf x, _) = extractTree ts

tail' :: BRAList a -> BRAList a
tail' = snd . extractTree

getAt :: BRAList a -> Int -> a
getAt (t:ts) i = if i < size t then lookupTree t i
                 else getAt ts (i - size t)

lookupTree :: Tree a -> Int -> a
lookupTree (Leaf x) 0 = x
lookupTree (Node sz t1 t2) i = if i < sz `div` 2 then lookupTree t1 i
                               else lookupTree t2 (i - sz `div` 2)

setAt :: BRAList a -> Int -> a -> BRAList a
setAt (t:ts) i x = if i < size t then (updateTree t i x):ts
                   else t:setAt ts (i-size t) x

updateTree :: Tree a -> Int -> a -> Tree a
updateTree (Leaf _) 0 x = Leaf x
updateTree (Node sz t1 t2) i x = 
    if i < sz `div` 2 then Node sz (updateTree t1 i x) t2
    else Node sz t1 (updateTree t2 (i - sz `div` 2) x)

-- Auxilary functions for flatten etc.

fromList :: [a] -> BRAList a
fromList = foldr cons []

toList :: BRAList a -> [a]
toList [] = []
toList (t:ts) = (treeToList t) ++ toList ts where
    treeToList (Leaf x) = [x]
    treeToList (Node _ t1 t2) = (treeToList t1) ++ treeToList t2

-- testing
prop_cons :: [Int] -> Bool
prop_cons xs = xs == (toList $ fromList xs)

prop_head :: [Int] -> Property
prop_head xs = not (null xs) ==> xs == (rebuild $ fromList xs) where
    rebuild [] = []
    rebuild ts = head' ts : (rebuild $ tail' ts)

prop_lookup :: [Int] -> Int -> Property
prop_lookup xs i = (0 <=i && i < length xs) ==> (getAt (fromList xs) i) == (xs !! i)

prop_update :: [Int] -> Int -> Int -> Property
prop_update xs i y = (0 <=i && i< length xs) ==> toList (setAt (fromList xs) i y) == xs' where
    xs' = as ++ [y] ++ bs
    (as, (_:bs)) = splitAt i xs


-- Reference
-- [1]. Chris Okasaki. ``Purely Functional Random-Access Lists''. Functional Programming Languages and Computer Architecutre, June 1995, pages 86-95.

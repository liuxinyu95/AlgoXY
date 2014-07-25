{-
    RAList.hs, Random Access List
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

module RAList where

import Test.QuickCheck

-- Based on Chris Okasaki's ``Purely Functional Data structures'', 
--   Chapter 9, Numeric representation.

-- Binary tree representation

data Tree a = Leaf a
            | Node Int (Tree a) (Tree a) -- size, left, right
              deriving (Show)

-- Numeric representation, Binary number
data Digit a = Zero
             | One (Tree a) deriving (Show)

-- The random access list is represented as a forest of binary trees.
type RAList a = [Digit a]

-- Auxilary functions
size :: Tree a -> Int
size (Leaf _) = 1
size (Node sz _ _) = sz

-- Precondition: rank t1 = rank t2
link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

cons :: a -> RAList a -> RAList a
cons x ts = insertTree ts (Leaf x) 

insertTree :: RAList a -> Tree a -> RAList a
insertTree [] t = [One t]
insertTree (Zero:ts) t = One t : ts
insertTree (One t' :ts) t = Zero : insertTree ts (link t t')

-- Assert the RAList isn't empty
extractTree :: RAList a -> (Tree a, RAList a)
extractTree [One t] = (t, [])
extractTree (One t:ts) = (t, Zero:ts)
extractTree (Zero:ts) = (t1, One t2:ts') where
    (Node _ t1 t2, ts') = extractTree ts

head' :: RAList a -> a
head' ts = x where (Leaf x, _) = extractTree ts

tail' :: RAList a -> RAList a
tail' ts = ts' where (_, ts') = extractTree ts

getAt :: RAList a -> Int -> a
getAt (Zero:ts) i = getAt ts i
getAt (One t:ts) i = if i < size t then lookupTree t i
                     else getAt ts (i - size t)

lookupTree :: Tree a -> Int -> a
lookupTree (Leaf x) 0 = x
lookupTree (Node sz t1 t2) i = if i < sz `div` 2 then lookupTree t1 i
                               else lookupTree t2 (i - sz `div` 2)

setAt :: RAList a -> Int -> a -> RAList a
setAt (Zero:ts) i x = Zero:setAt ts i x
setAt (One t:ts) i x = if i < size t then One (updateTree t i x):ts
                       else One t:setAt ts (i-size t) x

updateTree :: Tree a -> Int -> a -> Tree a
updateTree (Leaf _) 0 x = Leaf x
updateTree (Node sz t1 t2) i x = 
    if i < sz `div` 2 then Node sz (updateTree t1 i x) t2
    else Node sz t1 (updateTree t2 (i - sz `div` 2) x)

-- Auxiliary functions for flatten etc.

fromList :: [a] -> RAList a
fromList = foldr cons []

toList :: RAList a -> [a]
toList [] = []
toList (Zero:ts) = toList ts
toList (One t:ts) = (treeToList t) ++ toList ts where
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

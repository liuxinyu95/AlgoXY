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

size (Leaf _) = 1
size (Node sz _ _) = sz

link t1 t2 = Node (size t1 + size t2) t1 t2

insert x = insertTree (Leaf x) where
  insertTree t [] = [t]
  insertTree t (t':ts) = if size t < size t' then  t:t':ts
                         else insertTree (link t t') ts

-- Assert the list isn't empty
extract ((Leaf x):ts) = (x, ts)
extract ((Node _ t1 t2):ts) = extract (t1:t2:ts)

head' = fst . extract

tail' = snd . extract

getAt _ [] = Nothing
getAt i (t:ts) | i < 0 = Nothing
               | i < size t = lookupTree i t
               | otherwise = getAt (i - size t) ts
  where
    lookupTree 0 (Leaf x) = Just x
    lookupTree i (Node sz t1 t2) | i < sz `div` 2 = lookupTree i t1
                                 | otherwise = lookupTree (i - sz `div` 2) t2

setAt (t:ts) i x = if i < size t then (updateTree t i x):ts
                   else t:setAt ts (i-size t) x
  where
    updateTree (Leaf _) 0 x = Leaf x
    updateTree (Node sz t1 t2) i x =
        if i < sz `div` 2 then Node sz (updateTree t1 i x) t2
        else Node sz t1 (updateTree t2 (i - sz `div` 2) x)

fromList = foldr insert []

toList [] = []
toList (t:ts) = (treeToList t) ++ toList ts where
    treeToList (Leaf x) = [x]
    treeToList (Node _ t1 t2) = (treeToList t1) ++ treeToList t2

-- testing
prop_insert :: [Int] -> Bool
prop_insert xs = xs == (toList $ fromList xs)

prop_head :: [Int] -> Property
prop_head xs = not (null xs) ==> xs == (rebuild $ fromList xs) where
    rebuild [] = []
    rebuild ts = head' ts : (rebuild $ tail' ts)

prop_lookup :: [Int] -> Int -> Bool
prop_lookup xs i = case getAt i (fromList xs) of
  Nothing -> i < 0 || i >= length xs
  Just x -> x == (xs !! i)

prop_update :: [Int] -> Int -> Int -> Property
prop_update xs i y = (0 <=i && i< length xs) ==> toList (setAt (fromList xs) i y) == xs' where
    xs' = as ++ [y] ++ bs
    (as, (_:bs)) = splitAt i xs

testAll = do
  quickCheck prop_insert
  quickCheck prop_head
  quickCheck prop_lookup
  quickCheck prop_update

-- Reference
-- [1]. Chris Okasaki. ``Purely Functional Random-Access Lists''. Functional Programming Languages and Computer Architecutre, June 1995, pages 86-95.

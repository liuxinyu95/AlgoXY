{-
 BSTree.hs
 Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
 
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

module BSTree where

import Test.QuickCheck -- QuickCheck v1 is used when this program created.
import qualified Data.List as L -- for verification purpose only
import Prelude hiding(lookup, min, max)

data Tree a = Empty 
            | Node (Tree a) a (Tree a) deriving (Show, Eq)

-- Helper functions for tree
leaf::a -> Tree a
leaf a = Node Empty a Empty

left::Tree a -> Tree a
left (Node l _ _) = l
left _ = Empty

right::Tree a -> Tree a
right (Node _ _ r) = r
right _ = Empty

key::Tree a -> Maybe a
key (Node _ k _) = Just k
key _ = Nothing

isEmpty::Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- in-order tree traverse
mapT::(a->b) -> Tree a -> Tree b
mapT _ Empty = Empty
mapT f (Node l x r)= Node (mapT f l) (f x) (mapT f r)

-- Lookup in tree
lookup::(Ord a)=> Tree a -> a -> Tree a
lookup Empty _ = Empty
lookup t@(Node l k r) x | k == x = t
                        | x < k = lookup l x
                        | otherwise = lookup r x

-- Tree Min
min::Tree a -> a
min (Node Empty x _) = x
min (Node l _ _) = min l

-- Tree Max
max::Tree a -> a
max (Node _ x Empty) = x
max (Node _ _ r) = max r

-- Insert an element into a tree
insert::(Ord a) => Tree a -> a -> Tree a
insert Empty k = Node Empty k Empty
insert (Node l x r) k | k < x = Node (insert l k) x r
                      | otherwise = Node l x (insert r k)

-- Delete an element from a tree
-- The algorithm described in CLRS is not used here, I used the algorithm
-- which is mentioned in Annotated STL, P 235 (by Hou Jie)
--   if x has only one child: just splice x out
--   if x has two children: use min(right) to replce x
delete::(Ord a)=> Tree a -> a -> Tree a
delete Empty _ = Empty
delete (Node l k r) x | x < k = (Node (delete l x) k r)
                      | x > k = (Node l k (delete r x))
                      -- x == k
                      | isEmpty l = r
                      | isEmpty r = l
                      | otherwise = (Node l k' (delete r k')) where k' = min r

-- Traverse a part of tree inside a range [a, b]
mapR :: (Ord a)=>(a->b) -> a -> a -> Tree a -> Tree b
mapR f a b t = map' t where
    map' Empty = Empty
    map' (Node l k r) | k < a = map' r
                      | a <= k && k <= b = Node (map' l) (f k) (map' r)
                      | k > b = map' l
               

-- Helper to build a binary search tree from a list
fromList::(Ord a)=>[a] -> Tree a
fromList = foldl insert Empty

toList::(Ord a)=>Tree a -> [a]
toList Empty = []
toList (Node l x r) = toList l ++ [x] ++ toList r

-- test
prop_build :: (Show a)=>(Ord a)=>[a] -> Bool
prop_build xs = L.sort xs == (toList $ fromList xs)

prop_map :: (Ord a) => [a] -> Bool
prop_map xs = mapT id (fromList xs) == fromList xs

prop_lookup :: (Ord a) => [a] -> a -> Bool
prop_lookup xs x = f $ key $ lookup (fromList xs) x where
    f Nothing  = not $ elem x xs
    f (Just y) = x == y

prop_min :: (Ord a, Num a) => [a] -> Property
prop_min xs = not (null xs) ==> minimum xs == min (fromList xs)

prop_max :: (Ord a, Num a) => [a] -> Property
prop_max xs = not (null xs) ==> maximum xs == max (fromList xs)

prop_del :: (Ord a, Num a) => [a] -> a -> Bool
prop_del xs x = L.sort (L.delete x xs) == toList (delete (fromList xs) x)

prop_mapR :: (Ord a, Num a) =>[a] -> a -> a -> Bool
prop_mapR xs a b = filter (\x-> a<= x && x <=b) (L.sort xs) ==
                   toList (mapR id a b (fromList xs))

testAll = do 
  quickCheck (prop_build::[Int]->Bool)
  quickCheck (prop_map::[Int]->Bool)
  quickCheck (prop_lookup::[Int]->Int->Bool)
  quickCheck (prop_min::[Int]->Property)
  quickCheck (prop_max::[Int]->Property)
  quickCheck (prop_del::[Int]->Int->Bool)
  quickCheck (prop_mapR::[Int]->Int->Int->Bool)

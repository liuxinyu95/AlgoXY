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

import Test.QuickCheck
import Data.List (sort)
import Prelude hiding(lookup, min, max)

data Tree a = Empty 
            | Node (Tree a) a (Tree a) deriving (Show)

-- Helper functions for tree
leaf::a -> Tree a
leaf a = Node Empty a Empty

left::Tree a -> Tree a
left (Node l _ _) = l
left _ = Empty

right::Tree a -> Tree a
right (Node _ _ r) = r
right _ = Empty

key::Tree a -> a
key (Node _ k _) = k

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
max (Node _ _ r) = min r

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
prop_build :: (Ord a)=>[a] -> Bool
prop_build xs = sort xs == (toList $ fromList xs)

prop_mapR :: (Ord a) =>[a] -> a -> a -> Bool
prop_mapR xs a b = filter (\x-> a<= x && x <=b) (sort xs) ==
                   toList (mapR id a b (fromList xs))

-- test data
t1 = leaf 4
t2 = fromList [15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9]

-- test tree creation
testBuildTree = "\ntest create empty:\t"++ show (Empty::Tree Int)++
                "\ntest create leaf:\t" ++ show t1 ++ 
                "\ntest create from list:\t" ++ show t2

-- test tree walk
testTreeWalk = "\ntest tree in-order walk by apply (-):\t"++show (mapT negate t2)

-- test min/max for tree
testMinMax = "\ntest min leaf:\t" ++  show (min t1) ++
             "\ntest min tree:\t" ++ show (min t2) ++ 
             "\ntest max leaf:\t" ++ show (max t1) ++
             "\ntest max tree:\t" ++ show (max t2)

-- test lookup in tree
testLookup = "\ntest lookup empty tree:\t"++ show (lookup (Empty::Tree Int) 3) ++
             "\ntest lookup in leaf:\t"++ show (lookup t1 4)++
             "\ntest lookup non exist value in leaf:\t" ++ show (lookup t1 5)++
             "\ntest lookup non exist node in tree:\t"++ show (lookup t2 5)++
             "\ntest lookup a node in tree:\t"++ show (lookup t2 18)

testDel = "\ntest del 17:\t"++ show (delete t2 17)++
          "\ntest del 7:\t"++ show (delete t2 7)++
          "\ntest del 6:\t" ++ show (delete t2 6)++
          "\ntest del 15:\t"++ show (delete t2 15)++
          "\ntest del non-exist:\t" ++ show (delete t2 5)

oldTest = do
    putStrLn testBuildTree
    putStrLn testMinMax
    putStrLn testLookup
    putStrLn testTreeWalk
    putStrLn testDel

--newTest = do
--    test prop_build
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

-- Search in tree
search::(Ord a)=> Tree a -> a -> Tree a
search Empty _ = Empty
search t@(Node l k r) x | k == x = t
                        | x < k = search l x
                        | otherwise = search r x

-- Tree Min
mint::Tree a -> Tree a
mint t = if isEmpty (left t) then t else mint $ left t

-- Tree Max
maxt::Tree a -> Tree a
maxt t = if isEmpty (right t) then t else maxt $ right t

-- Below functions are low efficiency compare to their imperactive implementation

-- Find parent of a value in tree
-- The performance of parent is low: O(log n)
parent::(Ord a)=>Tree a -> a -> Tree a
parent Empty _ = Empty
parent t x | x < (key t) = if isParent (left t) x then t else parent (left t) x
           | x > (key t) = if isParent (right t) x then t else parent (right t) x
           | otherwise = Empty where
               isParent Empty _ = False
               isParent t x = (key t)==x

-- Find an ancestor of a value wich match a certain rule
-- Performance is very low.
findAncestor::(Ord a)=>Tree a -> a ->(a->a->Bool)-> Tree a
findAncestor t x f = checkParent t (parent t x) x f where
    checkParent _ Empty _ _ = Empty
    checkParent t p x f = if f (key p) x then p
                          else checkParent t (parent t (key p)) x f


-- successor
-- Performance is very low.
succt::(Ord a)=>Tree a -> a -> Tree a
succt Empty _ = Empty
succt t x = if not $ isEmpty rightNode
            then mint rightNode
            else findAncestor t x (>) where
                rightNode = right (search t x)

-- predecessor
-- Performance is very low
predt::(Ord a)=>Tree a -> a -> Tree a
predt Empty _ = Empty
predt t x = if not $ isEmpty leftNode
            then maxt leftNode
            else findAncestor t x (<) where
                leftNode = left (search t x)

-- End of low efficiency functions
              
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
                      | otherwise = (Node l k' (delete r k')) where k' = key $ mint r

-- Helper to build a binary search tree from a list
fromList::(Ord a)=>[a] -> Tree a
fromList = foldl insert Empty

toList::(Ord a)=>Tree a -> [a]
toList Empty = []
toList (Node l x r) = toList l ++ [x] ++ toList r

-- test
prop_build :: (Ord a)=>[a] -> Bool
prop_build xs = sort xs == (toList $ fromList xs)

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
testMinMax = "\ntest min empty:\t" ++ (show $ mint (Empty::Tree Int)) ++
             "\ntest min leaf:\t" ++  show (mint t1) ++
             "\ntest min tree:\t" ++ show (mint t2) ++ 
             "\ntest max empty:\t" ++ (show $ maxt (Empty::Tree Int)) ++
             "\ntest max leaf:\t" ++ show (maxt t1) ++
             "\ntest max tree:\t" ++ show (maxt t2)

-- test search in tree
testSearch = "\ntest search empty tree:\t"++ show (search (Empty::Tree Int) 3) ++
             "\ntest search in leaf:\t"++ show (search t1 4)++
             "\ntest search non exist value in leaf:\t" ++ show (search t1 5)++
             "\ntest search non exist node in tree:\t"++ show (search t2 5)++
             "\ntest search a node in tree:\t"++ show (search t2 18)

-- test succ/pred
testSuccPred = "\ntest succ of 7:\t"++ show (key (succt t2 7)) ++
               "\ntest parent of 7:\t"++ show (key (parent t2 7))++
               "\ntest parent of 13:\t"++ show (key (parent t2 13))++
               "\ntest bigger ancestor of 7:\t"++ show (key (findAncestor t2 7 (>)))++
               "\ntest bigger ancestor of 13:\t"++ show (key (findAncestor t2 13 (>)))++
               "\ntest succ of 13:\t"++ show (key (succt t2 13)) ++
               "\ntest pred of 6:\t"++ show (key (predt t2 6))++
               "\ntest pred of 7:\t"++ show (key (predt t2 7))

testDel = "\ntest del 17:\t"++ show (delete t2 17)++
          "\ntest del 7:\t"++ show (delete t2 7)++
          "\ntest del 6:\t" ++ show (delete t2 6)++
          "\ntest del 15:\t"++ show (delete t2 15)++
          "\ntest del non-exist:\t" ++ show (delete t2 5)

oldTest = do
    putStrLn testBuildTree
    putStrLn testMinMax
    putStrLn testSearch
    putStrLn testTreeWalk
    putStrLn testSuccPred
    putStrLn testDel

--newTest = do
--    test prop_build
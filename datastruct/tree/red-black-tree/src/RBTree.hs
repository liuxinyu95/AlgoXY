-- RBTree.hs, Haskell Red-Black tree implementation
-- Copyright (C) 2010 Liu Xinyu (liuxinyu95@gmail.com)
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- Most of the work comes from Okasaki's work in [1]

module RBTree where

import Test.QuickCheck -- QuickCheck v1
import qualified Data.List as L -- for verification purpose only
import Prelude hiding (min)

data Color = R | B | BB deriving (Show, Eq) -- BB is doubly black, used for deletion
data RBTree a = Empty
              | Node Color (RBTree a) a (RBTree a)
              | BBEmpty -- doubly black empty

-- Tree min, same as the one defined in BST
min::RBTree a -> a
min (Node _ Empty x _) = x
min (Node _ l _ _) = min l

isEmpty :: (RBTree a) -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Insertion/Deletion
insert::(Ord a)=>RBTree a -> a -> RBTree a
insert t x = makeBlack $ ins t where  --[1]
    ins Empty = Node R Empty x Empty  --[2]
    ins (Node color l k r)
        | x < k     = balance color (ins l) k r
        | otherwise = balance color l k (ins r) --[3]
    makeBlack(Node _ l k r) = Node B l k r

{- footnotes
[1]: Always set the root color as black;
[2]: new node is inserted as a red leaf, 
     to reserve property 1, 3, 5, only property 2, 4 may be violated
[3]: All keys should be indentical -}

-- Core function for insert, to make the tree balanced after insertion.
-- refer to paper of [Chris Okasaki]
balance::Color -> RBTree a -> a -> RBTree a -> RBTree a
balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d) -- case 1
balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d) -- case 2
balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d) -- case 3
balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d) -- case 4
balance color l k r = Node color l k r

delete::(Ord a)=>RBTree a -> a -> RBTree a
delete t x = blackenRoot (del t x) where
    del Empty _ = Empty
    del (Node color l k r) x
        | x < k = fixDB color (del l x) k r
        | x > k = fixDB color l k (del r x)
        -- x == k, delete this node
        | isEmpty l = if color==B then makeBlack r else r
        | isEmpty r = if color==B then makeBlack l else l
        | otherwise = fixDB color l k' (del r k') where k'= min r
    blackenRoot (Node _ l k r) = Node B l k r
    blackenRoot _ = Empty

makeBlack::RBTree a -> RBTree a
makeBlack (Node B l k r) = Node BB l k r -- doubly black
makeBlack (Node _ l k r) = Node B l k r
makeBlack Empty = BBEmpty
makeBlack t = t

-- Core function for delete, to solve the uniform black height violation.
-- refer to CLRS
fixDB::Color -> RBTree a -> a -> RBTree a -> RBTree a
fixDB color BBEmpty k Empty = Node BB Empty k Empty
fixDB color BBEmpty k r = Node color Empty k r
fixDB color Empty k BBEmpty = Node BB Empty k Empty
fixDB color l k BBEmpty = Node color l k Empty
-- the sibling is black, and it has one red child
fixDB color a@(Node BB _ _ _) x (Node B (Node R b y c) z d) = Node color (Node B (makeBlack a) x b) y (Node B c z d)
fixDB color a@(Node BB _ _ _) x (Node B b y (Node R c z d)) = Node color (Node B (makeBlack a) x b) y (Node B c z d)
fixDB color (Node B a x (Node R b y c)) z d@(Node BB _ _ _) = Node color (Node B a x b) y (Node B c z (makeBlack d))
fixDB color (Node B (Node R a x b) y c) z d@(Node BB _ _ _) = Node color (Node B a x b) y (Node B c z (makeBlack d))
-- the sibling and its 2 children are all black, propagate the blackness up
fixDB color a@(Node BB _ _ _) x (Node B b@(Node B _ _ _) y c@(Node B _ _ _))
    = makeBlack (Node color (makeBlack a) x (Node R b y c))
fixDB color (Node B a@(Node B _ _ _) x b@(Node B _ _ _)) y c@(Node BB _ _ _)
    = makeBlack (Node color (Node R a x b) y (makeBlack c))
-- the sibling is red
fixDB B a@(Node BB _ _ _) x (Node R b y c) = fixDB B (fixDB R a x b) y c
fixDB B (Node R a x b) y c@(Node BB _ _ _) = fixDB B a x (fixDB R b y c)
-- otherwise
fixDB color l k r = Node color l k r

-- Auxiliary functions

fromList::(Ord a)=>[a] -> RBTree a
fromList = foldl insert Empty

toList :: (Ord a) => RBTree a -> [a]
toList Empty = []
toList (Node _ l x r) = toList l ++ [x] ++ toList r

-- For pretty printing
instance Show a => Show (RBTree a) where
    show Empty = "."
    show (Node c l k r) = "(" ++ show l ++ " " ++ 
                          show k ++ ":" ++ show c ++ " " ++ 
                          show r ++ ")"

-- test

isRootBlack :: (RBTree a) -> Bool
isRootBlack Empty = True
isRootBlack (Node B _ _ _) = True
isRootBlack _ = False

adjacentRed :: (RBTree a) -> Bool
adjacentRed Empty = False
adjacentRed (Node R l@(Node R _ _ _) _ _) = True
adjacentRed (Node R _ _ r@(Node R _ _ _)) = True
adjacentRed (Node _ l _ r) = adjacentRed l || adjacentRed r

eqBlack :: (RBTree a) -> Bool
eqBlack Empty = True
eqBlack (Node _ Empty _ r) = eqBlack r
eqBlack (Node _ l _ Empty) = eqBlack l
eqBlack (Node _ l _ r) = and [eqBlack l, eqBlack r, blackness l == blackness r]

blackness :: (RBTree a) -> Int
blackness Empty = 0
blackness (Node R l _ r) = blackness l `max` blackness r
blackness (Node B l _ r) = 1 + (blackness l `max` blackness r)

isRedBlack :: (RBTree a) -> Bool
isRedBlack t = and [isRootBlack t, not $ adjacentRed t, eqBlack t]

prop_bst :: (Ord a, Num a) => [a] -> Bool
prop_bst xs = (L.sort $ L.nub xs) == (toList $ fromList $ L.nub xs)

prop_insert_redblack :: (Ord a, Num a) => [a] -> Bool
prop_insert_redblack = isRedBlack . fromList . L.nub

prop_del :: (Ord a, Num a) => [a] -> a -> Bool
prop_del xs x = L.sort (L.delete x xs) == toList (delete (fromList xs) x)

prop_del_redblack :: (Ord a, Num a) => [a] -> a -> Bool
prop_del_redblack xs x = isRedBlack $ delete (fromList xs) x

testAll = do
  quickCheck (prop_bst::[Int]->Bool)
  quickCheck (prop_insert_redblack::[Int]->Bool)
  quickCheck (prop_del::[Int]->Int->Bool)
  quickCheck (prop_del_redblack::[Int]->Int->Bool)

{- Reference 
[Chris Okasaki] Functional Pearls Red-Black trees in a functional setting.
   J. Functional programming. Jan. 1999.
   http://www.eecs.usma.edu/webs/people/okasaki/jfp99.ps-}

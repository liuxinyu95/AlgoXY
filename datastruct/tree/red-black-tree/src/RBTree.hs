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

import Test.QuickCheck
import qualified Data.List as L -- for verification purpose only
import Prelude hiding (min)

data Color = R | B | BB deriving (Show, Eq) -- BB is doubly black for deletion
data RBTree a = Empty
              | Node Color (RBTree a) a (RBTree a)
              | BBEmpty -- doubly black empty

min (Node _ Empty x _) = x
min (Node _ l _ _) = min l

isEmpty Empty = True
isEmpty _ = False

insert x = makeBlack . ins x where
    ins x Empty = Node R Empty x Empty
    ins x (Node color l k r)
        | x < k     = balance color (ins x l) k r
        | otherwise = balance color l k (ins x r)
    makeBlack (Node _ l k r) = Node B l k r

balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d) -- case 1
balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d) -- case 2
balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d) -- case 3
balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d) -- case 4
balance color l k r = Node color l k r

delete x = makeBlack . del x where
    del x Empty = Empty
    del x (Node color l k r)
        | x < k = fixDB color (del x l) k r
        | x > k = fixDB color l k (del x r)
        -- x == k, delete this node
        | isEmpty l = if color == B then shiftBlack r else r
        | isEmpty r = if color == B then shiftBlack l else l
        | otherwise = fixDB color l m (del m r) where m = min r
    makeBlack (Node _ l k r) = Node B l k r
    makeBlack _ = Empty

shiftBlack (Node B l k r) = Node BB l k r
shiftBlack (Node _ l k r) = Node B  l k r
shiftBlack Empty = BBEmpty
shiftBlack BBEmpty = Empty

isDB (Node BB _ _ _) = True
isDB BBEmpty = True
isDB _ = False

-- To solve the uniform black height violation.
fixDB::Color -> RBTree a -> a -> RBTree a -> RBTree a
-- Case 1: The sibling of the doubly-black nod is black, and it has a red sub-tree.
fixDB color a@(Node BB _ _ _) x (Node B (Node R b y c) z d) = Node color (Node B (shiftBlack a) x b) y (Node B c z d)
fixDB color BBEmpty x (Node B (Node R b y c) z d) = Node color (Node B Empty x b) y (Node B c z d)
fixDB color a@(Node BB _ _ _) x (Node B b y (Node R c z d)) = Node color (Node B (shiftBlack a) x b) y (Node B c z d)
fixDB color BBEmpty x (Node B b y (Node R c z d)) = Node color (Node B Empty x b) y (Node B c z d)
fixDB color (Node B a x (Node R b y c)) z d@(Node BB _ _ _) = Node color (Node B a x b) y (Node B c z (shiftBlack d))
fixDB color (Node B a x (Node R b y c)) z BBEmpty = Node color (Node B a x b) y (Node B c z Empty)
fixDB color (Node B (Node R a x b) y c) z d@(Node BB _ _ _) = Node color (Node B a x b) y (Node B c z (shiftBlack d))
fixDB color (Node B (Node R a x b) y c) z BBEmpty = Node color (Node B a x b) y (Node B c z Empty)
-- Case 2: The sibling of the doubly-black node is red
fixDB B a@(Node BB _ _ _) x (Node R b y c) = fixDB B (fixDB R a x b) y c
fixDB B a@BBEmpty x (Node R b y c) = fixDB B (fixDB R a x b) y c
fixDB B (Node R a x b) y c@(Node BB _ _ _) = fixDB B a x (fixDB R b y c)
fixDB B (Node R a x b) y c@BBEmpty = fixDB B a x (fixDB R b y c)
-- Case 3: The sibling of the doubly-black node, and its two sub-trees are all black.
--         Propagate the blackness up.
fixDB color a@(Node BB _ _ _) x (Node B b y c) = shiftBlack (Node color (shiftBlack a) x (Node R b y c))
fixDB color BBEmpty x (Node B b y c) = shiftBlack (Node color Empty x (Node R b y c))
fixDB color (Node B a x b) y c@(Node BB _ _ _) = shiftBlack (Node color (Node R a x b) y (shiftBlack c))
fixDB color (Node B a x b) y BBEmpty = shiftBlack (Node color (Node R a x b) y Empty)
-- otherwise
fixDB color l k r = Node color l k r

-- Auxiliary functions

fromList::(Ord a)=>[a] -> RBTree a
fromList = foldr insert Empty

toList :: (Ord a) => RBTree a -> [a]
toList Empty = []
toList (Node _ l x r) = toList l ++ [x] ++ toList r

-- For pretty printing
instance Show a => Show (RBTree a) where
    show BBEmpty = "*"
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

eqBlack t = blackness t > 0

blackness Empty = 1
blackness (Node c l _ r) = if a /= b then -1000 else (a + (ord c))
  where
    a = blackness l
    b = blackness r
    ord B = 1
    ord _ = 0

isRedBlack :: (RBTree a) -> Bool
isRedBlack t = and [isRootBlack t, not $ adjacentRed t, eqBlack t]

prop_bst :: (Ord a, Num a) => [a] -> Bool
prop_bst xs = let xs' = L.nub xs in
  (L.sort xs') == (toList $ fromList xs')

prop_insert_redblack :: (Ord a, Num a) => [a] -> Bool
prop_insert_redblack = isRedBlack . fromList . L.nub

prop_del :: (Ord a, Num a) => [a] -> a -> Bool
prop_del xs x = let xs' = L.nub xs in
  L.sort (L.delete x xs') == toList (delete x (fromList xs'))

prop_del_redblack :: (Ord a, Num a) => [a] -> a -> Bool
prop_del_redblack xs x = let xs' = L.nub xs in
  isRedBlack $ delete x (fromList xs')

testAll = do
  quickCheck (prop_bst::[Int]->Bool)
  quickCheck (prop_insert_redblack::[Int]->Bool)
  quickCheck (prop_del::[Int]->Int->Bool)
  quickCheck (prop_del_redblack::[Int]->Int->Bool)

{- Reference
[Chris Okasaki] Functional Pearls Red-Black trees in a functional setting.
   J. Functional programming. Jan. 1999.
   http://www.eecs.usma.edu/webs/people/okasaki/jfp99.ps-}

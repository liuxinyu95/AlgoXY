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
import qualified Data.List as L -- for verification purpose
import Prelude hiding(min, max)

-- alternative to define tree Foldable:
--   {-# LANGUAGE DeriveFoldable #-}
--   This provide null, fold, elem automatically

data Tree a = Empty
            | Node (Tree a) a (Tree a) deriving (Show, Eq)

leaf a = Node Empty a Empty

left (Node l _ _) = l
left _ = Empty

right (Node _ _ r) = r
right _ = Empty

key (Node _ k _) = Just k
key _ = Nothing

isEmpty Empty = True
isEmpty _ = False

foldt _ _ z Empty = z
foldt f g z (Node l k r) = g (foldt f g z l) (f k) (foldt f g z r)

-- in-order traverse
mapt _ Empty = Empty
mapt f (Node l x r)= Node (mapt f l) (f x) (mapt f r)

maptr :: (a -> b) -> Tree a -> Tree b
maptr f = foldt f Node Empty

-- plain fold, drop the tree structure, i.e. can't implement map
fold _ z Empty = z
fold f z (Node l k r) = fold f (k `f` (fold f z r)) l

member _ Empty = False
member x (Node l k r) | x == k = True
                     | x < k = member x l
                     | otherwise = member x r

-- lookupt :: (Eq a, Ord a) => a -> Tree (a, b) -> Maybe b
lookupt _ Empty = Nothing
lookupt x (Node l (k, v) r) | k == x = Just v
                            | x < k = lookupt x l
                            | otherwise = lookupt x r

min (Node Empty k _) = k
min (Node l _ _) = min l

max (Node _ k Empty) = k
max (Node _ _ r) = max r

insert x Empty = Node Empty x Empty
insert x (Node l k r) | x < k = Node (insert x l) k r
                      | otherwise = Node l k (insert x r)

-- Delete an element from a tree
--   if x has only one child: just splice x out
--   if x has two children: use min(right) to replace x
delete _ Empty = Empty
delete x (Node l k r) | x < k = Node (delete x l) k r
                      | x > k = Node l k (delete x r)
                      | otherwise = del l r
  where
    del Empty r = r
    del l Empty = l
    del l r = let k' = min r in Node l k' (delete k' r)

-- Traverse a part of tree inside a range [a, b]
mapR f a b t = map' t where
    map' Empty = Empty
    map' (Node l k r) | k < a = map' r
                      | a <= k && k <= b = Node (map' l) (f k) (map' r)
                      | k > b = map' l

fromList::(Ord a) => [a] -> Tree a
fromList = foldr insert Empty

toList Empty = []
toList (Node l k r) = toList l ++ [k] ++ toList r

-- tree sort
tsort :: (Eq a, Ord a) => [a] -> [a]
tsort = toList . fromList

preOrder Empty = []
preOrder (Node l k r) = k : preOrder l ++ preOrder r

postOrder Empty = []
postOrder (Node l k r) = postOrder l ++ postOrder r ++ [k]

-- Rebuild the binary tree from pre-order/in-order traverse list
rebuild [] _ = Empty
rebuild [c] _ = leaf c
rebuild (x:xs) ins = Node (rebuild prl inl) x (rebuild prr inr) where
  (inl, _:inr) = (takeWhile (/= x) ins, dropWhile (/=x) ins)
  (prl, prr) = splitAt (length inl) xs

-- test
prop_build :: (Ord a) => [a] -> Bool
prop_build xs = L.sort xs == tsort xs

prop_map :: (Ord a) => [a] -> Bool
prop_map xs = mapt id (fromList xs) == fromList xs

prop_map1 :: (Ord a) => [a] -> Bool
prop_map1 xs = maptr id (fromList xs) == fromList xs

prop_fold :: (Ord a, Num a) => [a] -> Bool
prop_fold xs = fold (+) 0 (fromList xs) == sum xs

prop_elem :: (Ord a) => [a] -> a -> Bool
prop_elem xs x = (x `elem` xs) == (x `member` (fromList xs))

prop_lookup :: (Ord a) => [a] -> a -> Bool
prop_lookup xs x = (lookupt x (fromList kvs)) == (lookup x kvs) where
  kvs = zip (L.nub xs) [1..]

prop_min :: (Ord a, Num a) => [a] -> Property
prop_min xs = not (null xs) ==> minimum xs == min (fromList xs)

prop_max :: (Ord a, Num a) => [a] -> Property
prop_max xs = not (null xs) ==> maximum xs == max (fromList xs)

prop_del :: (Ord a, Num a) => [a] -> a -> Bool
prop_del xs x = L.sort (L.delete x xs) == toList (delete x (fromList xs))

prop_mapR :: (Ord a, Num a) =>[a] -> a -> a -> Bool
prop_mapR xs a b = filter (\x-> a<= x && x <=b) (L.sort xs) ==
                   toList (mapR id a b (fromList xs))

prop_rebuild :: (Ord a, Num a) => [a] -> Bool
prop_rebuild xs = tr == rebuild prs ins where
  tr = fromList xs
  prs = preOrder tr
  ins = toList tr

testAll = do
  quickCheck (prop_build::[Int]->Bool)
  quickCheck (prop_map::[Int]->Bool)
  quickCheck (prop_map1::[Int]->Bool)
  quickCheck (prop_fold::[Int]->Bool)
  quickCheck (prop_elem::[Int]->Int->Bool)
  quickCheck (prop_lookup::[Int]->Int->Bool)
  quickCheck (prop_min::[Int]->Property)
  quickCheck (prop_max::[Int]->Property)
  quickCheck (prop_del::[Int]->Int->Bool)
  quickCheck (prop_mapR::[Int]->Int->Int->Bool)
  quickCheck (prop_rebuild::[Int]->Bool)

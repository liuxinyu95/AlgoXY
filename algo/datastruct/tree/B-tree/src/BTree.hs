{-
    BTree.hs, B-Tree in Haskell.
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

module BTree where

import Data.List (intersperse)

data BTree a = Node{ keys :: [a]
                   , children :: [BTree a]
                   , degree :: Int} deriving (Eq, Show)

empty deg = Node [] [] deg

full::BTree a -> Bool
full tr = (length $ keys tr) >= 2*(degree tr)-1

insert :: (Ord a)=> BTree a -> a -> BTree a
insert tr x = fix $ ins tr where
    ins (Node ks [] t) = Node ([a|a<-ks, a<x]++[x]++[b|b<-ks, b>x]) [] t
    ins (Node (k:ks) (c:cs) t)
        | x < k = merge (ins $ Node [] [c] t) (Node (k:ks) cs t)
        | otherwise = merge (Node [k] [c] t) (ins $ Node ks cs t)
    ins (Node [] [c] t) = let c' = ins c in if full c' then split c' else Node [] [c'] t --fix $ Node [] [ins c] t

fix :: BTree a -> BTree a
fix (Node [] [tr] _) = if full tr then split tr else tr -- Shrink height
fix tr = if full tr then split tr else tr

merge :: BTree a -> BTree a -> BTree a
merge (Node ks1 cs1 t) (Node ks2 cs2 _) = fix $ Node (ks1++ks2) (cs1++cs2) t

-- take n [] == drop n[] == []
split :: BTree a -> BTree a
split (Node ks cs t) = Node [k] [c1, c2] t where
    c1 = Node (take (t-1) ks) (take t cs) t
    c2 = Node (drop t ks) (drop t cs) t
    k  = head (drop (t-1) ks)

toString :: (Show a)=>BTree a -> String
toString (Node ks [] _) = "("++(concat $ intersperse "," (map show ks))++")"
toString tr = "("++(toStr (keys tr) (children tr))++")" where
    toStr (k:ks) (c:cs) = (toString c)++", "++(show k)++", "++(toStr ks cs)
    toStr [] [c] = toString c

listToBTree::(Ord a)=>[a]->Int->BTree a
listToBTree lst t = foldl insert (empty t) lst

--test
testInsert = toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 3
       
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

import Data.List (intersperse, partition)

data BTree a = Node{ keys :: [a]
                   , children :: [BTree a]
                   , degree :: Int} deriving (Eq, Show)

empty deg = Node [] [] deg

full::BTree a -> Bool
full tr = (length $ keys tr) >= 2*(degree tr)-1

-- take n [] == drop n[] == []
insert :: (Ord a)=> BTree a -> a -> BTree a
insert (Node ks [] t) x = fix $ Node ([a|a<-ks, a<x]++[x]++[b|b<-ks, b>x]) [] t
insert (Node ks cs t) x = fix $ merge left (insert c x) right
    where
      left  = (ks' , cs' )
      right = (ks'', cs'')
      (ks', ks'') = partition (< x) ks
      cs' = take (length ks') cs
      cs''= drop (length cs' +1) cs
      c   = head $ drop (length cs') cs

fix :: BTree a -> BTree a
--fix (Node [] [tr] _) = if full tr then split tr else tr -- Shrink height
fix tr = if full tr then split tr else tr

split :: BTree a -> BTree a
split (Node ks cs t) = Node [k] [c1, c2] t where
    c1 = Node (take (t-1) ks) (take t cs) t
    c2 = Node (drop t ks) (drop t cs) t
    k  = head (drop (t-1) ks)

merge :: ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
merge (ks', cs') (Node [k] cs t) (ks'', cs'') = Node (ks'++[k]++ks'') (cs'++cs++cs'') t
merge (ks', cs') c (ks'', cs'') = Node (ks'++ks'') (cs'++[c]++cs'') (degree c)

toString :: (Show a)=>BTree a -> String
toString (Node ks [] _) = "("++(concat $ intersperse "," (map show ks))++")"
toString tr = "("++(toStr (keys tr) (children tr))++")" where
    toStr (k:ks) (c:cs) = (toString c)++", "++(show k)++", "++(toStr ks cs)
    toStr [] [c] = toString c

listToBTree::(Ord a)=>[a]->Int->BTree a
listToBTree lst t = foldl insert (empty t) lst

--test
testInsert = (toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 3) ++"\n"++
             (toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 2)
       
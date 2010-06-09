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

import qualified Data.List as L

data BTree a = Node{ keys :: [a]
                   , children :: [BTree a]
                   , degree :: Int} deriving (Eq, Show)

empty deg = Node [] [] deg

full::BTree a -> Bool
full tr = (length $ keys tr) >= 2*(degree tr)-1

low::BTree a -> Bool
low tr = (length $ keys tr) < (degree tr)-1

-- take n [] == drop n[] == []
insert :: (Ord a)=> BTree a -> a -> BTree a
insert (Node ks [] t) x = fix $ Node (L.insert x ks) [] t
insert (Node ks cs t) x = fix $ merge left (insert c x) right
    where
      left  = (ks' , cs' )
      right = (ks'', cs'')
      (ks', ks'') = L.partition (< x) ks
      (cs', (c:cs'')) = L.splitAt (length ks') cs

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
merge (ks', cs') c (ks'', cs'') 
    | low c = borrow (ks', cs') c (ks'', cs'')
    | otherwise = Node (ks'++ks'') (cs'++[c]++cs'') (degree c)

borrow :: ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
borrow ([], []) (Node ks cs t) ((k':ks'), (c':cs')) = 
    Node ks' ((Node (ks++[k']) (cs++[c']) t):cs') t
borrow (ks'@(k':_), cs'@(c':_)) (Node ks cs t) (ks'', cs'') = 
    Node ((init ks')++ks'') 
         ((init cs')++[Node ((last ks'):ks) ((last cs'):cs) t]++cs'') t
borrow _ c _ = c

delete :: (Ord a)=> BTree a -> a -> BTree a
delete (Node ks [] t) x = Node (L.delete x ks) [] t
delete (Node ks cs t) x = merge left (delete c x) right
    where
      (ks', ks'') = L.partition (<x) ks
      (cs', cs'') = L.splitAt (length ks') cs
      left = (ks', cs')
      right = if head ks'' == x then (tail ks'', drop 2 cs'')
              else (ks'', tail cs'')
      c = if head ks'' == x 
          then fix $ Node ((keys c1)++(keys c2)) ((children c1)++(children c2)) t
          else head cs''

toString :: (Show a)=>BTree a -> String
toString (Node ks [] _) = "("++(L.intercalate "," (map show ks))++")"
toString tr = "("++(toStr (keys tr) (children tr))++")" where
    toStr (k:ks) (c:cs) = (toString c)++", "++(show k)++", "++(toStr ks cs)
    toStr [] [c] = toString c

listToBTree::(Ord a)=>[a]->Int->BTree a
listToBTree lst t = foldl insert (empty t) lst

--test
testInsert = (toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 3) ++"\n"++
             (toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 2)

--testDelete = foldl delete (listToBTree "GMPXACDEJKNORSTUVYZ" 3) lst
       
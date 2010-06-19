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
import Control.Monad (foldM)

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
insert tr x = fixRoot $ ins tr where
    ins (Node ks [] t) = Node (L.insert x ks) [] t
    ins (Node ks cs t) = make (ks', cs') (ins c) (ks'', cs'')
    (ks', ks'') = L.partition (< x) (keys tr)
    (cs', (c:cs'')) = L.splitAt (length ks') (children tr)

fixRoot :: BTree a -> BTree a
fixRoot (Node [] [tr] _) = tr -- shrink height
fixRoot tr = if full tr then Node [k] [c1, c2] (degree tr) 
             else tr 
    where
      (c1, k, c2) = split tr

split :: BTree a -> (BTree a, a, BTree a)
split (Node ks cs t) = (c1, k, c2) where
    c1 = Node (take (t-1) ks) (take t cs) t
    c2 = Node (drop t ks) (drop t cs) t
    k  = head (drop (t-1) ks)

unsplit :: BTree a -> a -> BTree a -> BTree a
unsplit c1 k c2 = Node ((keys c1)++[k]++(keys c2))
                       ((children c1)++(children c2)) (degree c1)

-- merge two nodes into one
--  k1, k2, ..., kn          k1', k2', ..., km'
--C1, C2, ..., Cn, C{n+1}  C1',C2', ..., Cm', C{m+1}
-- recursively merge C{n+1} and C1' until both nodes are leaves
merge :: BTree a -> BTree a -> BTree a
merge (Node ks [] t) (Node ks' [] _) = Node (ks++ks') [] t
merge (Node ks cs t) (Node ks' cs' _) = Node (ks++ks') 
                                        ((init cs)++[merge (last cs) (head cs')]++(tail cs')) t

-- delete x from a B-tree
-- case 1:
--  k1, k2,     ..., x,   ...,   kn
--C1, C2, ..., C{i-1}, Ci,..., Cn, C{n+1}
-- x devides the nodes into 3 parts: left, x, and right, merge left and right
--
-- case 2:
-- k{i-1}<x<ki
-- new node is:
--  k1, k2, ..., k{i-1}, ki, ...,   kn
--C1, C2, ..., C{i-1}, C'i, C{i+1}, ..., Cn, C{n+1}
-- where C'i = recursive delte x from Ci
-- then fix if keys are too little in C'i
delete :: (Ord a)=> BTree a -> a -> BTree a
delete tr x = fixRoot $ del tr where
    del (Node ks [] t) = Node (L.delete x ks) [] t
    del (Node ks cs t) = 
        case L.elemIndex x ks of
          Just i -> merge (Node (take i ks) (take (i+1) cs) t) 
                          (Node (drop (i+1) ks) (drop (i+1) cs) t)
          Nothing -> make (ks', cs') (del c) (ks'', cs'')
    (ks', ks'') = L.partition (<x) (keys tr)
    (cs', (c:cs'')) = L.splitAt (length ks') (children tr)

make :: (Eq a)=>([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
make (ks', cs') c (ks'', cs'')
    | full c = fixFull (ks', cs') c (ks'', cs'')
    | low c  = fixLow  (ks', cs') c (ks'', cs'')
    | otherwise = Node (ks'++ks'') (cs'++[c]++cs'') (degree c)

fixFull :: ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
fixFull (ks', cs') c (ks'', cs'') = Node (ks'++[k]++ks'')
                                         (cs'++[c1,c2]++cs'') (degree c)
    where
      (c1, k, c2) = split c

fixLow :: (Eq a)=>([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
fixLow (ks', cs') c (ks'', cs'')
    | ks' /= [] = make (init ks', init cs') 
                  (unsplit (last cs') (last ks') c) 
                  (ks'', cs'')
    | ks''/= [] = make (ks', cs')
                  (unsplit c (head ks'') (head cs''))
                  (tail ks'', tail cs'')
    | otherwise = c

toString :: (Show a)=>BTree a -> String
toString (Node ks [] _) = "("++(L.intercalate "," (map show ks))++")"
toString tr = "("++(toStr (keys tr) (children tr))++")" where
    toStr (k:ks) (c:cs) = (toString c)++", "++(show k)++", "++(toStr ks cs)
    toStr [] [c] = toString c

listToBTree::(Ord a)=>[a]->Int->BTree a
listToBTree lst t = foldl insert (empty t) lst

--test
testInsert = do
  putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 3
--  putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 2 BUG!!! stack overflow!

testDelete = foldM delShow (listToBTree "GMPXACDEJKNORSTUVYZ" 3) "GAMUE" where
    delShow tr x = do
      let tr' = delete tr x
      putStrLn $ "delete "++(show x)
      putStrLn $ toString tr'
      return tr'

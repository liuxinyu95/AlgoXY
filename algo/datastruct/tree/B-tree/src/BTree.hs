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
insert (Node ks [] t) x = fix $ Node (L.insert x ks) [] t
insert (Node ks cs t) x = fix $ merge left (insert c x) right
    where
      left  = (ks' , cs' )
      right = (ks'', cs'')
      (ks', ks'') = L.partition (< x) ks
      (cs', (c:cs'')) = L.splitAt (length ks') cs


fix :: BTree a -> BTree a
fix tr = if full tr then split tr else tr

split :: BTree a -> BTree a
split (Node ks cs t) = Node [k] [c1, c2] t where
    c1 = Node (take (t-1) ks) (take t cs) t
    c2 = Node (drop t ks) (drop t cs) t
    k  = head (drop (t-1) ks)


unsplit :: BTree a -> BTree a
unsplit (Node [k] [c1, c2] t) = Node ((keys c1)++[k]++(keys c2))
                                ((children c1)++(children c2)) t


merge :: ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
merge (ks', cs') (Node [k] cs@(_:_) t) (ks'', cs'') = Node (ks'++[k]++ks'') (cs'++cs++cs'') t
merge (ks', cs') c (ks'', cs'') 
    | low c = borrow (ks', cs') c (ks'', cs'')
    | full c = merge (ks', cs') (split c) (ks'', cs'')
    | otherwise = Node (ks'++ks'') (cs'++[c]++cs'') (degree c)


borrow :: ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
borrow ([], []) c ([], []) = c
borrow ([], []) c ((k:ks), (c':cs)) = merge ([], []) 
                                      (fix $ unsplit $ Node [k] [c, c'] (degree c))
                                      (ks, cs)
borrow (ks, cs) c (ks', cs') = merge (init ks, init cs)
                               (fix $ unsplit $ Node [last ks] [last cs, c] (degree c))
                               (ks' ,cs')


delete :: (Ord a)=> BTree a -> a -> BTree a
delete (Node ks [] t) x = Node (L.delete x ks) [] t
delete (Node ks cs t) x = 
    case L.elemIndex x ks of
      Just i -> fix $ merge (take i ks, take i cs)
                            (delete (unsplit $ Node [x] [cs !! i, cs !! (i+1)] t) x)
                            (drop (i+1) ks, drop (i+2) cs)
      Nothing -> fix $ merge left (delete c x) right
    where
      left = (ks', cs')
      right = (ks'', cs'')
      (ks', ks'') = L.partition (<x) ks
      (cs', (c:cs'')) = L.splitAt (length ks') cs

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
  putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 2

testDelete = foldM delShow (listToBTree "GMPXACDEJKNORSTUVYZ" 3) "GAMUE" where
    delShow tr x = do
      let tr' = delete tr x
      putStrLn $ "delete "++(show x)
      putStrLn $ toString tr'
      return tr'
       
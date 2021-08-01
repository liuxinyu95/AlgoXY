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
import Control.Monad (foldM_, mapM_)

-- Definition
data BTree a = BTree { keys :: [a]
                     , subTrees :: [BTree a]
                     , degree :: Int } deriving (Eq, Show)

empty t = BTree [] [] t

full (BTree ks _ t) = (length ks) > 2 * t - 1

low (BTree ks _ t) = (length ks) < t - 1

insert :: (Ord a) => BTree a -> a -> BTree a
insert tr x = fixRoot $ ins tr x

ins :: (Ord a) => BTree a -> a -> BTree a
ins (BTree ks [] t) x = BTree (L.insert x ks) [] t
ins (BTree ks cs t) x = make (ks', cs') (ins c x) (ks'', cs'')
    where
      (ks', ks'') = L.partition (<x) ks
      (cs', (c:cs'')) = L.splitAt (length ks') cs

delete :: (Ord a) => BTree a -> a -> BTree a
delete tr x = fixRoot $ del tr x

del:: (Ord a) => BTree a -> a -> BTree a
del (BTree ks [] t) x = BTree (L.delete x ks) [] t
del (BTree ks cs t) x =
    case L.elemIndex x ks of
      Just i -> merge (BTree (take i ks) (take (i+1) cs) t)
                      (BTree (drop (i+1) ks) (drop (i+1) cs) t)
      Nothing -> make (ks', cs') (del c x) (ks'', cs'')
    where
      (ks', ks'') = L.partition (<x) ks
      (cs', (c:cs'')) = L.splitAt (length ks') cs

-- Search

search :: (Ord a)=> BTree a -> a -> Maybe (BTree a, Int)
search tr@(BTree ks cs _) k
    | matchFirst k $ drop len ks = Just (tr, len)
    | otherwise = if null cs then Nothing
                  else search (cs !! len) k
    where
      matchFirst x (y:_) = x==y
      matchFirst x _ = False
      len = length $ filter (<k) ks

-- Tree manipulation

split :: BTree a -> (BTree a, a, BTree a)
split (BTree ks cs t) = (c1, k, c2) where
    c1 = BTree (take (t-1) ks) (take t cs) t
    c2 = BTree (drop t ks) (drop t cs) t
    k  = head (drop (t-1) ks)

unsplit :: BTree a -> a -> BTree a -> BTree a
unsplit c1 k c2 = BTree ((keys c1)++[k]++(keys c2))
                       ((subTrees c1)++(subTrees c2)) (degree c1)

-- merge two nodes into one--  k1, k2, ..., kn          k1', k2', ..., km'
--C1, C2, ..., Cn, C{n+1}  C1',C2', ..., Cm', C{m+1}
-- recursively merge C{n+1} and C1' until both nodes are leaves
merge :: BTree a -> BTree a -> BTree a
merge (BTree ks [] t) (BTree ks' [] _) = BTree (ks++ks') [] t
merge (BTree ks cs t) (BTree ks' cs' _) = make (ks, init cs)
                                             (merge (last cs) (head cs'))
                                             (ks', tail cs')

--  Fixing B-tree balance property
fixRoot :: BTree a -> BTree a
fixRoot (BTree [] [tr] _) = tr -- shrink height
fixRoot tr = if full tr then BTree [k] [c1, c2] (degree tr)
             else tr
    where
      (c1, k, c2) = split tr

make :: ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
make (ks', cs') c (ks'', cs'')
    | full c = fixFull (ks', cs') c (ks'', cs'')
    | low c  = fixLow  (ks', cs') c (ks'', cs'')
    | otherwise = BTree (ks'++ks'') (cs'++[c]++cs'') (degree c)

fixFull :: ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
fixFull (ks', cs') c (ks'', cs'') = BTree (ks'++[k]++ks'')
                                         (cs'++[c1,c2]++cs'') (degree c)
    where
      (c1, k, c2) = split c

fixLow :: ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
fixLow (ks'@(_:_), cs') c (ks'', cs'') = make (init ks', init cs')
                                              (unsplit (last cs') (last ks') c)
                                              (ks'', cs'')
fixLow (ks', cs') c (ks''@(_:_), cs'') = make (ks', cs')
                                              (unsplit c (head ks'') (head cs''))
                                              (tail ks'', tail cs'')
fixLow _ c _ = c

toString :: (Show a)=>BTree a -> String
toString (BTree ks [] _) = "("++(L.intercalate ", " (map show ks))++")"
toString tr = "("++(toStr (keys tr) (subTrees tr))++")" where
    toStr (k:ks) (c:cs) = (toString c)++", "++(show k)++", "++(toStr ks cs)
    toStr [] [c] = toString c

listToBTree::(Ord a)=>[a]->Int->BTree a
listToBTree lst t = foldl insert (empty t) lst

--test
testInsert = do
  putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 3
  --testInsertV "GMPXACDEJKNORSTUVYZ" 3
  putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 2
  --testInsertV "GMPXACDEJKNORSTUVYZ" 2
  putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZBFHIQW" 3

-- verbose
testInsertV lst t =
  mapM_ (\x->putStrLn $ toString $ listToBTree x t) (tail $ L.reverse $ L.tails lst)

testDelete = foldM_ delShow (listToBTree "GMPXACDEJKNORSTUVYZBFHIQW" 3) "EGAMU" where
    delShow tr x = do
      let tr' = delete tr x
      putStrLn $ "delete "++(show x)
      putStrLn $ toString tr'
      return tr'

testSearch = mapM_ (showSearch (listToBTree lst 3)) $ lst++"L"
    where
      showSearch tr x = do
        case search tr x of
          Just (_, i) -> putStrLn $ "found" ++ (show x)
          Nothing -> putStrLn $ "not found" ++ (show x)
      lst = "GMPXACDEJKNORSTUVYZBFHIQW"

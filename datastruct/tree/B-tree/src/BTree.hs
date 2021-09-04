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
import Test.QuickCheck

data BTree a = BTree { keys :: [a]
                     , subTrees :: [BTree a] } deriving (Eq, Show)

empty = BTree [] []

full d (BTree ks _) = (length ks) > 2 * d - 1

low d (BTree ks _) = (length ks) < d - 1

insert :: (Ord a) => a -> (Int, BTree a) -> (Int, BTree a)
insert x (d, t) = fixRoot (d, ins t) where
    ins (BTree ks ts) | x `elem` ks = (BTree ks ts) -- skip duplicated key
                      | null ts = BTree (L.insert x ks) []
                      | otherwise = balance d (ks1, ts1) (ins t') (ks2, ts2) where
      (ks1, ks2) = L.partition (< x) ks
      (ts1, (t':ts2)) = L.splitAt (length ks1) ts

-- delete :: (Ord a) => BTree a -> a -> BTree a
-- delete tr x = fixRoot $ del tr x

-- del:: (Ord a) => BTree a -> a -> BTree a
-- del (BTree ks [] t) x = BTree (L.delete x ks) [] t
-- del (BTree ks cs t) x =
--     case L.elemIndex x ks of
--       Just i -> merge (BTree (take i ks) (take (i+1) cs) t)
--                       (BTree (drop (i+1) ks) (drop (i+1) cs) t)
--       Nothing -> balance (ks', cs') (del c x) (ks'', cs'')
--     where
--       (ks', ks'') = L.partition (<x) ks
--       (cs', (c:cs'')) = L.splitAt (length ks') cs

-- lookup :: (Ord a)=> a -> BTree a -> Maybe (BTree a, Int)
-- lookup tr@(BTree ks cs _) k
--     | matchFirst k $ drop len ks = Just (tr, len)
--     | otherwise = if null cs then Nothing
--                   else lookup (cs !! len) k
--     where
--       matchFirst x (y:_) = x==y
--       matchFirst x _ = False
--       len = length $ filter (<k) ks

split d (BTree ks ts) = (BTree ks1 ts1, k, BTree ks2 ts2) where
  (ks1, k:ks2) = L.splitAt (d - 1) ks
  (ts1, ts2) = L.splitAt d ts

unsplit (BTree ks1 ts1) k (BTree ks2 ts2) = BTree (ks1 ++ [k] ++ ks2)
                                                  (ts1 ++ ts2)

merge _ (BTree ks []) (BTree ks' [])  = BTree (ks ++ ks') []
merge d (BTree ks ts) (BTree ks' ts') = balance d (ks, init ts)
                                                (merge d (last ts) (head ts'))
                                                (ks', tail ts')

fixRoot (d, BTree [] [t]) = (d, t)
fixRoot (d, t) | full d t  = let (t1, k, t2) = split d t in
                               (d, BTree [k] [t1, t2])
               | otherwise = (d, t)

balance :: Int -> ([a], [BTree a]) -> BTree a -> ([a], [BTree a]) -> BTree a
balance d (ks1, ts1) t (ks2, ts2)
    | full d t  = fixFull
    | low  d t  = fixLow
    | otherwise = BTree (ks1 ++ ks2) (ts1 ++ [t] ++ ts2)
  where
    fixFull = let (t1, k, t2) = split d t in
                BTree (ks1 ++ [k] ++ ks2) (ts1 ++ [t1, t2] ++ ts2)
    fixLow | not $ null ks1 = balance d (init ks1, init ts1)
                                      (unsplit (last ts1) (last ks1) t)
                                      (ks2, ts2)
           | not $ null ks2 = balance d (ks1, ts1)
                                      (unsplit t (head ks2) (head ts2))
                                      (tail ks2, tail ts2)
           | otherwise = t


fromList d xs = foldr insert (d, empty) xs

toList (BTree ks []) = ks
toList (BTree ks (t:ts)) = concat ((toList t) : (zipWith (\k tr -> k : toList tr) ks ts))

-- Quick check

degOf [] = 2
degOf (x:_) = x `mod` 5 + 2 -- limit degree: 2 <= d < 6 for simplicity

isBTree :: Int -> Int -> (BTree a) -> Bool
isBTree d n t@(BTree _ ts) = (not $ full d t)
                          && (n == 0 || (not $ low d t))
                          && (and [isBTree d (n + 1) tr | tr <- ts])

prop_order :: [Int] -> Bool
prop_order xs = (L.sort $ L.nub xs) == (toList $ snd $ fromList d xs) where
  d = degOf xs

prop_insert :: [Int] -> Bool
prop_insert xs = isBTree d 0 $ snd $ fromList d xs where d = degOf xs

testAll = do
  quickCheck prop_order
  quickCheck prop_insert

-- testInsert = do
--   putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 3
--   --testInsertV "GMPXACDEJKNORSTUVYZ" 3
--   putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZ" 2
--   --testInsertV "GMPXACDEJKNORSTUVYZ" 2
--   putStrLn $ toString $ listToBTree "GMPXACDEJKNORSTUVYZBFHIQW" 3

-- Verbose
-- testInsertV lst t =
--   mapM_ (\x->putStrLn $ toString $ listToBTree x t) (tail $ L.reverse $ L.tails lst)

-- testDelete = foldM_ delShow (listToBTree "GMPXACDEJKNORSTUVYZBFHIQW" 3) "EGAMU" where
--     delShow tr x = do
--       let tr' = delete tr x
--       putStrLn $ "delete "++(show x)
--       putStrLn $ toString tr'
--       return tr'

-- testSearch = mapM_ (showSearch (listToBTree lst 3)) $ lst++"L"
--     where
--       showSearch tr x = do
--         case search tr x of
--           Just (_, i) -> putStrLn $ "found" ++ (show x)
--           Nothing -> putStrLn $ "not found" ++ (show x)
--       lst = "GMPXACDEJKNORSTUVYZBFHIQW"

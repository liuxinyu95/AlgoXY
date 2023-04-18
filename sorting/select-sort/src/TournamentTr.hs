-- TournamentTr.hs
-- Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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

-- Tournament tree based selection sort
-- [1] Donald E. Knuth. ``The Art of Computer Programming, Volume 3: Sorting and Searching (2nd Edition)''.
-- Addison-Wesley Professional; 2 edition (May 4, 1998) ISBN-10: 0201896850 ISBN-13: 978-0201896855

module TrounamentTr where

import Test.QuickCheck -- for verification
import Data.List (sort, sortBy) -- for verification

-- Note: in order to derive from Ord for free, the order must be:
--    negative infinity, regular, then positive infinity
data Infinite a = NegInf | Only a | Inf deriving (Eq, Show, Ord)

data Tr a = Empty | Br (Tr a) (Infinite a) (Tr a) deriving Show

key (Br _ k _ ) = k

wrap x = Br Empty (Only x) Empty

only (Only x) = x

minBy p a b = if p a b then a else b

merge p t1 t2 = Br t1 (minBy p (key t1) (key t2)) t2

fromList :: Ord a => [a] -> Tr a
fromList = fromListWith (<=)

-- fromListWith :: (Ord a) => (Infinite a -> Infinite a -> Bool) -> [a] -> Tr a
fromListWith p xs = build $ map wrap xs where
  build [] = Empty
  build [t] = t
  build ts = build $ pair ts
  pair (t1:t2:ts) = (merge p t1 t2) : pair ts
  pair ts = ts

pop :: Ord a => Tr a -> Tr a
pop = popWith (<=) Inf

popWith p inf = delMin where
  delMin (Br Empty _ Empty) = Br Empty inf Empty
  delMin (Br l k r) | k == key l = let l' = delMin l in Br l' (minBy p (key l') (key r)) r
                    | k == key r = let r' = delMin r in Br l (minBy p (key l) (key r')) r'

top = only . key

toList :: Ord a => Tr a -> [a]
toList = toListWith (<=) Inf

toListWith p inf = flat where
  flat Empty = []
  flat t | inf == key t = []
         | otherwise = (top t) : (flat $ popWith p inf t)

tsort = toList . fromList

-- tsortBy :: (Ord a) => (Infinite a -> Infinite a -> Bool) -> Infinite a -> [a] -> [a]
tsortBy p inf xs = toListWith p inf $ fromListWith p xs where

prop_tsort :: [Int] -> Bool
prop_tsort xs = (sort xs) == (tsort xs)

prop_tsort_des :: [Int] -> Bool
prop_tsort_des xs = (sortBy (flip compare) xs) == (tsortBy (>=) NegInf xs)

testAll = do
  quickCheck prop_tsort
  quickCheck prop_tsort_des

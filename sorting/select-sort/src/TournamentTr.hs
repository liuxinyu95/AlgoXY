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

import Test.QuickCheck -- for verification purpose only
import Data.List(sort) -- for verification purpose only

-- Note: in order to derive from Ord for free, the order must be: 
--    negative infinity, regular, then positive infinity
data Infinite a = NegInf | Only a | Inf deriving (Eq, Show, Ord)

only (Only x) = x

data Tr a = Empty | Br (Tr a) a (Tr a) deriving Show

key (Br _ k _ ) = k

wrap x = Br Empty (Only x) Empty

branch t1 t2 = Br t1 (min (key t1) (key t2)) t2

fromList :: (Ord a) => [a] -> Tr (Infinite a)
fromList = build . (map wrap) where
  build [] = Empty
  build [t] = t
  build ts = build $ pair ts 
  pair (t1:t2:ts) = (branch t1 t2):pair ts
  pair ts = ts
  
pop (Br Empty _ Empty) = Br Empty Inf Empty
pop (Br l k r) | k == key l = let l' = pop l in Br l' (min (key l') (key r)) r
               | k == key r = let r' = pop r in Br l (min (key l) (key r')) r'

top = only . key

tsort :: (Ord a) => [a] -> [a]
tsort = sort' . fromList where
    sort' Empty = []
    sort' (Br _ Inf _) = []
    sort' t = (top t) : (sort' $ pop t)

prop_tsort :: [Int]->Bool
prop_tsort xs = (sort xs) == (tsort xs)

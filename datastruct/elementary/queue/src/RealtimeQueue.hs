{-
    RealtimeQueue.hs, Realtime O(1) queue
    Copyright (C) 2012, Liu Xinyu (liuxinyu95@gmail.com)

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

-- Realtime O(1) Queue based on Hood-Melville Queue in [1].

module RealtimeQueue where

import Queue
import Test.QuickCheck

-- Definition
--   A Queue is consist of a front list f and a rear list r.
--   Add new element to tail, extract from head

--   State for increamental realization for f ++ reverse r
--   n: number of elements remaining in f
data State a = Empty
             | Reverse Int [a] [a] [a] [a] -- n, acc_f, f, acc_r, r
             | Concat Int [a] [a]          -- n, acc, rev_f
             | Done [a]  -- f' = f ++ reverse r
             deriving (Show, Eq)

-- f, n = length f, state, r, m = length r
data RealtimeQueue a = RTQ [a] Int (State a) [a] Int
                     deriving (Show, Eq)

-- Skip the empty error for pop and front
instance Queue RealtimeQueue where
    empty = RTQ [] 0 Empty [] 0

    isEmpty (RTQ _ n _ _ _) = n == 0

    push x (RTQ f n s r m) = balance f n s (x:r) (m + 1)     -- O(1)

    pop (RTQ (_:f) n s r m) = balance f (n - 1) (abort s) r m   -- O(1)

    front (RTQ (x:_) _ _ _ _) = x

balance f n s r m
    | m <= n =  step f n s r m
    | otherwise = step f (m + n) (next (Reverse 0 [] f [] r)) [] 0

-- stepped f ++ reverse r
step f n s r m = queue (next s) where
  queue (Done f') = RTQ f' n Empty r m
  queue s' = RTQ f n s' r m

-- Two stages f ++ reverse r
--  1. reverse xs = reverse' [] xs where
--        reverse' acc [] = acc
--        reverse' acc (x:xs) = reverse' (x:acc) xs
--  2. xs ++ ys = reverse' ys (reverse xs)
next (Reverse n f' (x:f) r' (y:r)) = Reverse (n + 1) (x:f') f (y:r') r
next (Reverse n f' [] r' [y]) = next $ Concat n (y:r') f'
next (Concat 0 acc _) = Done acc
next (Concat n acc (x:f')) = Concat (n-1) (x:acc) f'
next s = s

-- Abort unnecessary append as the element is popped
abort (Concat 0 (_:acc) _) = Done acc -- rollback 1 elem
abort (Concat n acc f') = Concat (n - 1) acc f'
abort (Reverse n f' f r' r) = Reverse (n - 1) f' f r' r
abort s = s

-- test
fromList :: [a] -> RealtimeQueue a
fromList = foldr push (empty::RealtimeQueue a)

prop_queue :: [Int] -> Bool
prop_queue xs = proc xs (empty::(RealtimeQueue Int)) == proc' xs []

--[1]. Chris Okasaki's ``Purely Functional Data structures''

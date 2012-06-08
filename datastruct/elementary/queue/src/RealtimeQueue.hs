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
--   A Queue is consist with two linked-list, front list and rear list.
--   Add new element in tail, while extract element from head

--   State for increamental realization for f ++ reverse r
--      field n: number of elements left in f
data State a = Empty 
             | Reverse Int [a] [a] [a] [a] -- n, f', acc_f' r, acc_r
             | Concat Int [a] [a]          -- n, rev_f', acc
             | Done [a] -- result: f ++ reverse r
               deriving (Show, Eq)

-- front, length of front, on-goint reverse state, rear, length of reverse
data RealtimeQueue a = RTQ [a] Int (State a) [a] Int
                     deriving (Show, Eq)

-- we skip the empty error for pop and front
instance Queue RealtimeQueue where
    empty = RTQ [] 0 Empty [] 0

    isEmpty (RTQ _ lenf _ _ _) = lenf == 0

    -- O(1) time push
    push (RTQ f lenf s r lenr) x = balance f lenf s (x:r) (lenr + 1)

    -- O(1) time pop
    pop (RTQ (_:f) lenf s r lenr) = balance f (lenf - 1) (abort s) r lenr

    front (RTQ (x:_) _ _ _ _) = x

balance f lenf s r lenr 
    | lenr <= lenf =  step f lenf s r lenr
    | otherwise = step f (lenf + lenr) (Reverse 0 f [] r []) [] 0

-- execute f ++ reverse r step by step
step f lenf s r lenr =
    case s' of 
      Done f' -> RTQ f' lenf Empty r lenr
      s' -> RTQ f lenf s' r lenr
    where s' = if null f then next $ next s else next s

-- realize of f ++ reverse r based on 2 increamental approaches
--  1. reverse xs = reverse' xs [] where
--        reverse' [] acc = acc
--        reverse' (x:xs) acc = reverse' xs (x:acc)
--  2. xs ++ ys = reverse' (reverse xs) ys
next (Reverse n (x:f) f' (y:r) r') = Reverse (n+1) f (x:f') r (y:r')
next (Reverse n [] f' [y] r') = Concat n f' (y:r')
next (Concat 0 _ acc) = Done acc
next (Concat n (x:f') acc) = Concat (n-1) f' (x:acc)
next s = s

-- Abort unnecessary appending as the element is popped
abort (Concat 0 _ (_:acc)) = Done acc -- Note! we rollback 1 elem
abort (Concat n f' acc) = Concat (n-1) f' acc
abort (Reverse n f f' r r') = Reverse (n-1) f f' r r'
abort s = s

-- test

fromList :: [a] -> RealtimeQueue a
fromList = foldl push (empty::RealtimeQueue a)

prop_queue :: [Int] -> Bool
prop_queue xs = proc xs (empty::(RealtimeQueue Int)) == proc' xs []

--[1]. Chris Okasaki's ``Purely Functional Data structures''
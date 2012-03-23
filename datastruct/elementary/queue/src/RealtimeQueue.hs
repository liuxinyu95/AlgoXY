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
--   TODO: Add explaination

data Reverse a = Empty 
               | Reverse [a] [a]
                 deriving (Show, Eq)

-- front, length of front, on-goint reverse state, rear, length of reverse
data RealtimeQueue a = RTQ [a] Int (Reverse a) [a] Int
                     deriving (Show, Eq)

-- Auxiliary function to create stepped reverse state
reverse' xs = Reverse xs []

-- we skip the empty error for pop and front
instance Queue RealtimeQueue where
    empty = RTQ [] 0 Empty [] 0

    isEmpty (RTQ _ lenf _ _ _) = lenf == 0

    -- Amortized O(1) time push
    push (RTQ f lenf s r lenr) x = balance f lenf s (x:r) (lenr + 1)

    -- Amortized O(1) time pop
    pop (RTQ (_:f) lenf s r lenr) = balance f (lenf - 1) s r lenr

    front (RTQ (x:_) _ _ _ _) = x

balance f lenf s r lenr 
    | lenr <= lenf =  step f lenf s r lenr
    | otherwise = step f (lenf + lenr) (reverse' r) [] 0

-- execute reverse step by step
step [] lenf (Reverse [] acc) r lenr = step acc lenf Empty r lenr
step [] lenf s r lenr = step [] lenf (next s) r lenr
step f lenf s r lenr = RTQ f lenf (next s) r lenr

next (Reverse (x:xs) acc) = Reverse xs (x:acc)
next s = s

-- test

prop_queue :: [Int] -> Bool
prop_queue xs = proc xs (empty::(RealtimeQueue Int)) == proc' xs []

--[1]. Chris Okasaki's ``Purely Functional Datastructures''
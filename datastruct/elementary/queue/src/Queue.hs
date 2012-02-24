{-
    Queue.hs, Batched Queue (FIFO)
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

-- Batched Queue based on Chris Okasaki's ``Purely Functional Datastructures''

module Queue where

import Test.QuickCheck

-- Definition
-- A Queue is consist with two linked-list, front list and rear list.
--   Add new element in tail, while extract element from head

type Queue a = ([a], [a])

-- Auxiliary function

empty = ([], [])

balance :: Queue a -> Queue a
balance ([], r) = (reverse r, [])
balance q = q

-- Amortized O(1) time push
push :: Queue a -> a -> Queue a
push (f, r) x = balance (f, x:r)

-- Amortized O(1) time pop
pop :: Queue a -> Queue a
pop ([], _) = error "Empty"
pop (_:f, r) =  balance (f, r)

front :: Queue a -> a
front ([], _) = error "Empty"
front (x:_, _) = x

isEmpty (f, _) = null f

-- test

proc :: [Int] -> Queue Int -> [Int]
proc [] _ = []
proc (x:xs) q | even x = proc xs (push q x)
              | isEmpty q = proc xs q
              | otherwise = front q : proc xs (pop q)

proc' :: [Int] -> [Int] -> [Int]
proc' [] _ = []
proc' (x:xs) lst | even x = proc' xs (lst ++ [x])
                 | null lst = proc' xs lst
                 | otherwise = head lst : proc' xs (tail lst)

prop_queue :: [Int] -> Bool
prop_queue xs = proc xs empty == proc' xs []
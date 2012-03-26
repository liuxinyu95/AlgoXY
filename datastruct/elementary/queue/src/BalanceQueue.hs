{-
    BalanceQueue.hs, Batched Queue (FIFO)
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

-- Balanced Batched Queue based on Banker's Queue and Physicist's Queue in [1]. 

module BalanceQueue where

import Queue
import Test.QuickCheck

-- Definition
--   A Queue is consist with two linked-list, front list and rear list.
--   Add new element in tail, while extract element from head
--   In order to make the queue balanced for pop/push, we force the invariant
--
--     length(rear list) < length(front list)
--

data BalanceQueue a = BQ [a] Int [a] Int

-- we skip the empty error for pop and front
instance Queue BalanceQueue where
    empty = BQ [] 0 [] 0

    isEmpty (BQ _ lenf _ _) = lenf == 0

    -- Amortized O(1) time push
    push (BQ f lenf r lenr) x = balance f lenf (x:r) (lenr + 1)

    -- Amortized O(1) time pop
    pop (BQ (_:f) lenf r lenr) = balance f (lenf - 1) r lenr

    front (BQ (x:_) _ _ _) = x

balance f lenf r lenr 
    | lenr <= lenf = BQ f lenf r lenr
    | otherwise = BQ (f ++ reverse r) (lenf + lenr) [] 0

-- test

prop_queue :: [Int] -> Bool
prop_queue xs = proc xs (empty::(BalanceQueue Int)) == proc' xs []

--[1]. Chris Okasaki's ``Purely Functional Datastructures''
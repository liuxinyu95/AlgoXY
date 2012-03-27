{-
    LazyRTQueue.hs, Realtime O(1) queue depends on lazy evaluation
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

module LazyRTQueue where

import Queue
import Test.QuickCheck

-- Definition
--   A Queue is consist with two linked-list, front list and rear list.
--   Add new element in tail, while extract element from head

data LazyRTQueue a = LQ [a] [a] [a] -- front, rear, f ++ reverse r
                     deriving (Show, Eq)

-- we skip the empty error for pop and front
instance Queue LazyRTQueue where
    empty = LQ [] [] []

    isEmpty (LQ f _ _) = null f

    -- O(1) time push
    push (LQ f r rot) x = balance f (x:r) rot

    -- O(1) time pop
    pop (LQ (_:f) r rot) = balance f r rot

    front (LQ (x:_) _ _) = x

balance f r [] = let f' = rotate f r [] in LQ f' [] f' 
balance f r (_:rot) = LQ f r rot

rotate [] [y] acc = y:acc
rotate (x:xs) (y:ys) acc = x : rotate xs ys (y:acc)

-- test

fromList :: [a] -> LazyRTQueue a
fromList = foldl push (empty::LazyRTQueue a)

prop_queue :: [Int] -> Bool
prop_queue xs = proc xs (empty::(LazyRTQueue Int)) == proc' xs []

--[1]. Chris Okasaki's ``Purely Functional Data structures''
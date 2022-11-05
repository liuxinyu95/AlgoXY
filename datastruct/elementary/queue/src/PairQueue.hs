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

module PairQueue where

import Queue
import Test.QuickCheck

-- Definition
-- A Queue is consist with two linked-list, front list and rear list.
--   Add new element in tail, while extract element from head

data PairQueue a = Q [a] [a]

-- Note that type PairQueue a = ([a], [a]) won't work.
-- Such type synonym can't be instance of Queue, even with
-- -XTypeSynonymInstances etc pragmas.
-- Neither we can use something like instance functor ((,) a) ...

instance Queue PairQueue where
    -- Auxiliary functions
    empty = Q [] []

    isEmpty (Q f _) = null f

    -- Amortized O(1) time push
    push x (Q f r) = balance f (x:r)

    -- Amortized O(1) time pop
    pop (Q [] _) = error "Empty"
    pop (Q (_:f) r) =  balance f r

    front (Q [] _) = error "Empty"
    front (Q (x:_) _) = x

balance [] r = Q (reverse r) []
balance f r = Q f r

-- test

prop_queue :: [Int] -> Bool
prop_queue xs = proc xs (empty::(PairQueue Int)) == proc' xs []

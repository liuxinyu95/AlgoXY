{-
    CList.hs, Catenable List based on Queue
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

-- Based on Chris Okasaki's ``Purely Functional Data structures'',
--    Section 10.2.1, Lists With Efficient Catenation

module CList where

import Test.QuickCheck -- for verification purpose only
import Prelude hiding (head, tail, (++))

-- import LazyRTQueue
--   We don't import lazy real-time queue, because it is stored separately
--   As it's short and easy, we duplicate the implementation here.
--   Please refer to the chapter about queue in AlgoXY.

-- A simple lazy real-time queue,

data Queue a = Q [a] [a] [a] deriving (Show, Eq) -- front, rear, stream of f ++ reverse r

emptyQ = Q [] [] []
isEmptyQ (Q f _ _) = null f

push (Q f r s) x = balance f (x:r) s

pop (Q (_:f) r s) = balance f r s

front (Q (x:_) _ _) = x

balance f r [] = let f' = rotate f r [] in Q f' [] f'
balance f r (_:s) = Q f r s

rotate [] [y] acc = y:acc
rotate (x:xs) (y:ys) acc = x : rotate xs ys (y:acc)

fold :: (a -> b -> b) -> b -> Queue a -> b
fold f z q | isEmptyQ q = z
           | otherwise = (front q) `f` fold f z (pop q)

-- Catenable list
data CList a = Empty | CList a (Queue (CList a)) deriving (Show, Eq)

singleton x = CList x emptyQ

x ++ Empty = x
Empty ++ y = y
(CList x q) ++ y = CList x (push q y)

isEmpty Empty = True
isEmpty _ = False

cons x xs = (singleton x) ++ xs
snoc xs x = xs ++ singleton x -- a.k.a append

head (CList x _) = x -- we skip the error handling for empty list
tail (CList _ q) = CList.concat q

-- folding right like function
concat = fold (++) Empty

-- concat' q | isEmptyQ q = Empty
--           | otherwise = link (front q) (concat' (pop q))

fromList :: [a] -> CList a
fromList = foldr cons Empty

toList :: CList a -> [a]
toList Empty = []
toList (CList x q) = x : fold (\e xs -> Prelude.concat [toList e, xs]) [] q

-- testing
prop_cons :: [Int] -> Bool
prop_cons xs = xs == (toList $ fromList xs)

prop_head :: [Int] -> Property
prop_head xs = not (null xs) ==> xs == (rebuild $ fromList xs) where
  rebuild Empty = []
  rebuild lst = head lst : (rebuild $ tail lst)

prop_concat :: [Int] -> [Int] -> Bool
prop_concat xs ys = Prelude.concat [xs, ys] == toList (fromList xs ++ fromList ys)

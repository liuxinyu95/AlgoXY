{-
    FingerTree.hs, Random access list by Finger Tree
    Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)

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

module FingerTree where

import Test.QuickCheck

-- Based on Ralf Hinze and Ross Paterson's work [1].

data Node a = Br2 a a | Br3 a a a deriving (Show)

data Tree a = Empty 
            | Lf a
            | Tr [a] (Tree (Node a)) [a]
              deriving (Show)

-- Examples:
-- Empty
-- Lf a
-- Tr [b] Empty [a]
-- Tr [e, d, c, b] Empty [a]
-- Tr [f, e] Lf (Br3 d c b) [a]
-- 

cons :: a -> Tree a -> Tree a
cons a Empty = Lf a
cons a (Lf b) = Tr [a] Empty [b]
cons a (Tr [b, c, d, e] m r) = Tr [a, b] (cons (Br3 c d e) m) r
cons a (Tr f m r) = Tr (a:f) m r

uncons :: Tree a -> (a, Tree a)
uncons (Lf a) = (a, Empty)
uncons (Tr [a] Empty [b]) = (a, Lf b)
uncons (Tr [a] Empty (r:rs)) = (a, Tr [r] Empty rs)
uncons (Tr [a] m r) = (a, Tr (nodeToList f) m' r) where (f, m') = uncons m
uncons (Tr f m r) = (head f, Tr (tail f) m r)

head' :: Tree a -> a
head' = fst . uncons

tail' :: Tree a -> Tree a
tail' = snd . uncons

snoc :: Tree a -> a -> Tree a
snoc Empty a = Lf a
snoc (Lf a) b = Tr [a] Empty [b]
snoc (Tr f m [a, b, c, d]) e = Tr f (snoc m (Br3 a b c)) [d, e]
snoc (Tr f m r) a = Tr f m (r++[a])

unsnoc :: Tree a -> (Tree a, a)
unsnoc (Lf a) = (Empty, a)
unsnoc (Tr [a] Empty [b]) = (Lf a, b)
unsnoc (Tr f@(_:_) Empty [a]) = (Tr (init f) Empty [last f], a)
unsnoc (Tr f m [a]) = (Tr f m' (nodeToList r), a) where (m', r) = unsnoc m
unsnoc (Tr f m r) = (Tr f m (init r), last r)

last' :: Tree a -> a
last' = snd . unsnoc

init' :: Tree a -> Tree a
init' = fst . unsnoc

-- auxiliary functions

fromList :: [a] -> Tree a
fromList = foldr cons Empty

toList :: Tree a -> [a]
toList Empty = []
toList t = (head' t):(toList $ tail' t)

nodeToList :: Node a -> [a]
nodeToList (Br2 a b) = [a, b]
nodeToList (Br3 a b c) = [a, b, c]

-- testing
prop_cons :: [Int] -> Bool
prop_cons xs = xs == (toList $ fromList xs)

prop_snoc :: [Int] -> Bool
prop_snoc xs = xs == (toList' $ foldl snoc Empty xs) where
    toList' Empty = []
    toList' t = (toList' $ init' t)++[last' t]

-- Reference
-- [1]. Ralf Hinze and Ross Paterson. ``Finger Trees: A Simple General-purpose Data Structure." in Journal of Functional Programming16:2 (2006), pages 197-217. http://www.soi.city.ac.uk/~ross/papers/FingerTree.html

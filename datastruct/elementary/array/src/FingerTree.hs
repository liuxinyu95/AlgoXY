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

data Node a = Br [a] deriving (Show)

data Tree a = Empty 
            | Lf a
            | Tr [a] (Tree (Node a)) [a]
              deriving (Show)

-- Operations at the front of the sequence

cons :: a -> Tree a -> Tree a
cons a Empty = Lf a
cons a (Lf b) = Tr [a] Empty [b]
cons a (Tr [b, c, d, e] m r) = Tr [a, b] (cons (Br [c, d, e]) m) r
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

-- Operations at the rear of the sequence

snoc :: Tree a -> a -> Tree a
snoc Empty a = Lf a
snoc (Lf a) b = Tr [a] Empty [b]
snoc (Tr f m [a, b, c, d]) e = Tr f (snoc m (Br [a, b, c])) [d, e]
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

-- Concatenation

concat' :: Tree a -> Tree a -> Tree a
concat' t1 t2 = merge t1 [] t2

merge :: Tree a -> [a] -> Tree a -> Tree a
merge Empty ts t2 = foldr cons t2 ts
merge t1 ts Empty = foldl snoc t1 ts
merge (Lf a) ts t2 = merge Empty (a:ts) t2
merge t1 ts (Lf a) = merge t1 (ts++[a]) Empty
merge (Tr f1 m1 r1) ts (Tr f2 m2 r2) = Tr f1 (merge m1 (nodes (r1 ++ ts ++ f2)) m2) r2

nodes :: [a] -> [Node a]
nodes [a, b] = [Br [a, b]]
nodes [a, b, c] = [Br [a, b, c]]
nodes [a, b, c, d] = [Br [a, b], Br [c, d]]
nodes (a:b:c:xs) = (Br [a, b, c]):nodes xs

-- auxiliary functions

fromList :: [a] -> Tree a
fromList = foldr cons Empty

toList :: Tree a -> [a]
toList Empty = []
toList t = (head' t):(toList $ tail' t)

nodeToList :: Node a -> [a]
nodeToList (Br xs) = xs

-- testing
prop_cons :: [Int] -> Bool
prop_cons xs = xs == (toList $ fromList xs)

prop_snoc :: [Int] -> Bool
prop_snoc xs = xs == (toList' $ foldl snoc Empty xs) where
    toList' Empty = []
    toList' t = (toList' $ init' t)++[last' t]

prop_concat :: [Int]->[Int]->Bool
prop_concat xs ys = (xs ++ ys) == (toList $ concat' (fromList xs) (fromList ys))

-- Reference
-- [1]. Ralf Hinze and Ross Paterson. ``Finger Trees: A Simple General-purpose Data Structure." in Journal of Functional Programming16:2 (2006), pages 197-217. http://www.soi.city.ac.uk/~ross/papers/FingerTree.html

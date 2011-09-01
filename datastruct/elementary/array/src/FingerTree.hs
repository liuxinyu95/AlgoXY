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

data Node a = Br Int [a] deriving (Show) -- size, branches

data Tree a = Empty 
            | Lf a
            | Tr Int [a] (Tree (Node a)) [a] -- size, front, middle, rear
              deriving (Show)

type FList a = Tree (Node a)

size :: Node a -> Int
size (Br s _) = s

sizeT :: FList a -> Int
sizeT Empty = 0
sizeT (Lf a) = size a
sizeT (Tr s _ _ _) = s

wrap :: a -> Node a
wrap x = Br 1 [x]

unwrap :: Node a -> a
unwrap (Br 1 [x]) = x

wraps :: [Node a] -> Node (Node a)
wraps xs = Br (sum $ map size xs) xs

unwraps :: Node a -> [a]
unwraps (Br _ xs) = xs

-- Operations at the front of the sequence

cons :: a -> FList a -> FList a
cons a t = cons' (wrap a) t

cons' :: (Node a) -> FList a -> FList a
cons' a Empty = Lf a
cons' a (Lf b) = Tr (size a + size b) [a] Empty [b]
cons' a (Tr s [b, c, d, e] m r) = Tr (s + size a) [a, b] (cons' (wraps [c, d, e]) m) r
cons' a (Tr s f m r) = Tr (s + size a) (a:f) m r

uncons :: FList a -> (a, FList a)
uncons ts = let (t, ts') = uncons' ts in (unwrap t, ts')

uncons' :: FList a -> ((Node a), FList a)
uncons' (Lf a) = (a, Empty)
uncons' (Tr _ [a] Empty [b]) = (a, Lf b)
uncons' (Tr s [a] Empty (r:rs)) = (a, Tr (s- size a) [r] Empty rs)
uncons' (Tr s [a] m r) = (a, Tr (s - size a) (unwraps f) m' r) where (f, m') = uncons' m
uncons' (Tr s (a:f) m r) = (a, Tr (s - size a) f m r)

head' :: FList a -> a
head' = fst . uncons

tail' :: FList a -> FList a
tail' = snd . uncons

-- Operations at the rear of the sequence

snoc :: FList a -> a -> FList a
snoc t a = snoc' t (wrap a)

snoc' :: FList a -> Node a -> FList a
snoc' Empty a = Lf a
snoc' (Lf a) b = Tr (size a + size b) [a] Empty [b]
snoc' (Tr s f m [a, b, c, d]) e = Tr (s + size e) f (snoc' m (wraps [a, b, c])) [d, e]
snoc' (Tr s f m r) a = Tr (s + size a) f m (r++[a])

unsnoc :: FList a -> (FList a, a)
unsnoc ts = let (ts', t) = unsnoc' ts in (ts', unwrap t)

unsnoc' :: FList a -> (FList a, (Node a))
unsnoc' (Lf a) = (Empty, a)
unsnoc' (Tr _ [a] Empty [b]) = (Lf a, b)
unsnoc' (Tr s f@(_:_) Empty [a]) = (Tr (s - size a) (init f) Empty [last f], a)
unsnoc' (Tr s f m [a]) = (Tr (s - size a) f m' (unwraps r), a) where (m', r) = unsnoc' m
unsnoc' (Tr s f m r) = (Tr (s - size a) f m (init r), a) where a = last r

last' :: FList a -> a
last' = snd . unsnoc

init' :: FList a -> FList a
init' = fst . unsnoc

-- Concatenation

concat' :: FList a -> FList a -> FList a
concat' t1 t2 = merge t1 [] t2

merge :: FList a -> [Node a] -> FList a -> FList a
merge Empty ts t2 = foldr cons' t2 ts
merge t1 ts Empty = foldl snoc' t1 ts
merge (Lf a) ts t2 = merge Empty (a:ts) t2
merge t1 ts (Lf a) = merge t1 (ts++[a]) Empty
merge (Tr s1 f1 m1 r1) ts (Tr s2 f2 m2 r2) = 
    Tr (s1 + s2 + (sum $ map size ts)) f1 (merge m1 (nodes (r1 ++ ts ++ f2)) m2) r2

nodes :: [Node a] -> [Node (Node a)]
nodes [a, b] = [Br (size a + size b) [a, b]]
nodes [a, b, c] = [Br (size a + size b + size c) [a, b, c]]
nodes [a, b, c, d] = [Br (size a + size b) [a, b], Br (size c + size d) [c, d]]
nodes (a:b:c:xs) = (Br (size a + size b + size c) [a, b, c]):nodes xs

-- auxiliary functions

fromList :: [a] -> FList a
fromList = foldr cons Empty

toList :: FList a -> [a]
toList Empty = []
toList t = (head' t):(toList $ tail' t)

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

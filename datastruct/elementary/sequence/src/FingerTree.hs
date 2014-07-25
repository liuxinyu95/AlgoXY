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

-- Auxiliary functions for calculate size of node and tree

size :: Node a -> Int
size (Br s _) = s

sizeL :: [Node a] -> Int
sizeL = sum .(map size)

sizeT :: FList a -> Int
sizeT Empty = 0
sizeT (Lf a) = size a
sizeT (Tr s _ _ _) = s

-- Auxiliary functions for building and unboxing node(s)

wrap :: a -> Node a
wrap x = Br 1 [x]

unwrap :: Node a -> a
unwrap (Br 1 [x]) = x

wraps :: [Node a] -> Node (Node a)
wraps xs = Br (sizeL xs) xs

unwraps :: Node a -> [a]
unwraps (Br _ xs) = xs

-- Helper function for building tree

tree :: [Node a] -> FList (Node a) -> [Node a] -> FList a
tree f Empty [] = foldr cons' Empty f
tree [] Empty r = foldr cons' Empty r
tree [] m r = let (f, m') = uncons' m in tree (unwraps f) m' r
tree f m [] = let (m', r) = unsnoc' m in tree f m' (unwraps r)
tree f m r = Tr (sizeL f + sizeT m + sizeL r) f m r

-- Operations at the front of the sequence

cons :: a -> FList a -> FList a
cons a t = cons' (wrap a) t

cons' :: (Node a) -> FList a -> FList a
cons' a Empty = Lf a
cons' a (Lf b) = tree [a] Empty [b]
cons' a (Tr _ [b, c, d, e] m r) = tree [a, b] (cons' (wraps [c, d, e]) m) r
cons' a (Tr _ f m r) = tree (a:f) m r

uncons :: FList a -> (a, FList a)
uncons ts = let (t, ts') = uncons' ts in (unwrap t, ts')

uncons' :: FList a -> ((Node a), FList a)
uncons' (Lf a) = (a, Empty)
uncons' (Tr _ [a] Empty [b]) = (a, Lf b)
uncons' (Tr _ [a] Empty (r:rs)) = (a, tree [r] Empty rs)
uncons' (Tr _ [a] m r) = (a, tree (unwraps f) m' r) where (f, m') = uncons' m
uncons' (Tr _ (a:f) m r) = (a, tree f m r)

head' :: FList a -> a
head' = fst . uncons

tail' :: FList a -> FList a
tail' = snd . uncons

-- Operations at the rear of the sequence

snoc :: FList a -> a -> FList a
snoc t a = snoc' t (wrap a)

snoc' :: FList a -> Node a -> FList a
snoc' Empty a = Lf a
snoc' (Lf a) b = tree [a] Empty [b]
snoc' (Tr _ f m [a, b, c, d]) e = tree f (snoc' m (wraps [a, b, c])) [d, e]
snoc' (Tr _ f m r) a = tree f m (r++[a])

unsnoc :: FList a -> (FList a, a)
unsnoc ts = let (ts', t) = unsnoc' ts in (ts', unwrap t)

unsnoc' :: FList a -> (FList a, (Node a))
unsnoc' (Lf a) = (Empty, a)
unsnoc' (Tr _ [a] Empty [b]) = (Lf a, b)
unsnoc' (Tr _ f@(_:_) Empty [a]) = (tree (init f) Empty [last f], a)
unsnoc' (Tr _ f m [a]) = (tree f m' (unwraps r), a) where (m', r) = unsnoc' m
unsnoc' (Tr _ f m r) = (tree f m (init r), (last r))

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
    Tr (s1 + s2 + (sizeL ts)) f1 (merge m1 (nodes (r1 ++ ts ++ f2)) m2) r2

nodes :: [Node a] -> [Node (Node a)]
nodes [a, b] = [wraps [a, b]]
nodes [a, b, c] = [wraps [a, b, c]]
nodes [a, b, c, d] = [wraps [a, b], wraps [c, d]]
nodes (a:b:c:xs) = (wraps [a, b, c]):nodes xs

-- Splitting

splitAt' :: Int -> FList a -> (FList a, Node a, FList a)
splitAt' _ (Lf x) = (Empty, x, Empty)
splitAt' i (Tr _ f m r) 
    | i < szf = let (xs, y, ys) = splitNodesAt i f
                in ((foldr cons' Empty xs), y, tree ys m r)
    | i < szf + szm = let (m1, t, m2) = splitAt' (i-szf) m
                          (xs, y, ys) = splitNodesAt (i-szf - sizeT m1) (unwraps t)
                      in (tree f m1 xs, y, tree ys m2 r)
    | otherwise = let (xs, y, ys) = splitNodesAt (i-szf -szm) r
                  in (tree f m xs, y, foldr cons' Empty ys)
    where
      szf = sizeL f
      szm = sizeT m

splitNodesAt :: Int -> [Node a] -> ([Node a], Node a, [Node a])
splitNodesAt 0 [x] = ([], x, [])
splitNodesAt i (x:xs) | i < size x = ([], x, xs)
                      | otherwise = let (xs', y, ys) = splitNodesAt (i-size x) xs 
                                    in (x:xs', y, ys)

-- Random access operations

getAt :: FList a -> Int -> a
getAt t i = unwrap x where (_, x, _) = splitAt' i t

extractAt :: FList a -> Int -> (a, FList a)
extractAt t i = let (l, x, r) = splitAt' i t in (unwrap x, concat' l r)

setAt :: FList a -> Int -> a -> FList a
setAt t i x = let (l, _, r) = splitAt' i t in concat' l (cons x r)

-- move the i-th element to front
moveToFront :: FList a -> Int -> FList a
moveToFront t i = let (a, t') = extractAt t i in cons a t'

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

prop_lookup :: [Int] -> Int -> Property
prop_lookup xs i = (0 <=i && i < length xs) ==> (getAt (fromList xs) i) == (xs !! i)

prop_update :: [Int] -> Int -> Int -> Property
prop_update xs i y = (0 <=i && i < length xs) ==> toList (setAt (fromList xs) i y) == xs' where
    xs' = as ++ [y] ++ bs
    (as, (_:bs)) = splitAt i xs

prop_mtf :: [Int] -> Int -> Property
prop_mtf xs i = (0 <=i && i < length xs) ==> (toList $ moveToFront (fromList xs) i) == mtf
    where
      mtf = b : as ++ bs
      (as, (b:bs)) = splitAt i xs


-- Reference
-- [1]. Ralf Hinze and Ross Paterson. ``Finger Trees: A Simple General-purpose Data Structure." in Journal of Functional Programming16:2 (2006), pages 197-217. http://www.soi.city.ac.uk/~ross/papers/FingerTree.html

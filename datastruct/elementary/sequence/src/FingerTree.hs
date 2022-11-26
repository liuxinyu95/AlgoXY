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

-- 2-3 tree
data Node a = Tr2 Int a a
            | Tr3 Int a a a
            deriving (Show)

-- Finger Tree
data Tree a = Empty
            | Lf a
            | Br Int [a] (Tree (Node a)) [a] -- size, front, mid, rear
            deriving (Show)

newtype Elem a = Elem { getElem :: a }
               deriving (Show)

newtype Seq a = Seq (Tree (Elem a))

class Sized a where
  size :: a -> Int

instance Sized (Elem a) where
  size _ = 1

instance Sized (Node a) where
  size (Tr2 s _ _) = s
  size (Tr3 s _ _ _) = s

instance Sized a => Sized (Tree a) where
  size Empty = 0
  size (Lf a) = size a
  size (Br s _ _ _) = s

tr2 a b = Tr2 (size a + size b) a b
tr3 a b c = Tr3 (size a + size b + size c) a b c

nodesOf (Tr2 _ a b) = [a, b]
nodesOf (Tr3 _ a b c) = [a, b, c]

(<|) :: a -> Seq a -> Seq a
x <| Seq xs = Seq (Elem x `cons` xs)

cons :: (Sized a) => a -> Tree a -> Tree a
cons a Empty = Lf a
cons a (Lf b) = Br (size a + size b) [a] Empty [b]
cons a (Br s [b, c, d, e] m r) = Br (s + size a) [a, b] ((tr3 c d e) `cons` m) r
cons a (Br s f m r) = Br (s + size a) (a:f) m r

head' (Seq xs) = getElem $ fst $ uncons xs

tail' (Seq xs) = Seq $ snd $ uncons xs

uncons :: (Sized a) => Tree a -> (a, Tree a)
uncons (Lf a) = (a, Empty)
uncons (Br _ [a] Empty [b]) = (a, Lf b)
uncons (Br s [a] Empty (r:rs)) = (a, Br (s - size a) [r] Empty rs)
uncons (Br s [a] m r) = (a, Br (s - size a) (nodesOf f) m' r) where (f, m') = uncons m
uncons (Br s (a:f) m r) = (a, Br (s - size a) f m r)

(|>) :: Seq a -> a -> Seq a
Seq xs |> x  = Seq (xs `snoc` Elem x)

snoc :: (Sized a) => Tree a -> a -> Tree a
snoc Empty a = Lf a
snoc (Lf a) b = Br (size a + size b) [a] Empty [b]
snoc (Br s f m [a, b, c, d]) e = Br (s + size e) f (m `snoc` (tr3 a b c)) [d, e]
snoc (Br s f m r) a = Br (s + size a) f m (r ++ [a])

last' (Seq xs) = getElem $ snd $ unsnoc xs

init' (Seq xs) = Seq $ fst $ unsnoc xs

unsnoc :: (Sized a) => Tree a -> (Tree a, a)
unsnoc (Lf a) = (Empty, a)
unsnoc (Br _ [a] Empty [b]) = (Lf a, b)
unsnoc (Br s f@(_:_:_) Empty [a]) = (Br (s - size a) (init f) Empty [last f], a)
unsnoc (Br s f m [a]) = (Br (s - size a) f m' (nodesOf r), a) where (m', r) = unsnoc m
unsnoc (Br s f m r) = (Br (s - size a) f m (init r), a) where a = last r

(+++) :: Seq a -> Seq a -> Seq a
Seq xs +++ Seq ys = Seq (merge xs [] ys)

merge :: (Sized a) => Tree a -> [a] -> Tree a -> Tree a
merge Empty es t2 = foldr cons t2 es
merge t1 es Empty = foldl snoc t1 es
merge (Lf a) es t2 = merge Empty (a:es) t2
merge t1 es (Lf a) = merge t1 (es++[a]) Empty
merge (Br s1 f1 m1 r1) es (Br s2 f2 m2 r2) =
    Br (s1 + s2 + (sum $ map size es)) f1 (merge m1 (trees (r1 ++ es ++ f2)) m2) r2

trees [a, b] = [tr2 a b]
trees [a, b, c] = [tr3 a b c]
trees [a, b, c, d] = [tr2 a b, tr2 c d]
trees (a:b:c:es) = (tr3 a b c):trees es

splitAt' :: (Sized a) => Int -> Tree a -> (Tree a, a, Tree a)
splitAt' _ (Lf x) = (Empty, x, Empty)
splitAt' i (Br s f m r)
    | i < szf = let (xs, y, ys) = splitNodesAt i f
                in ((foldr cons Empty xs), y, Br (s - i - 1) ys m r)
    | i < szf + szm = let (m1, t, m2) = splitAt' (i - szf) m
                          (xs, y, ys) = splitNodesAt (i- szf - size m1) (nodesOf t)
                      in (Br i f m1 xs, y, Br (s - i - 1) ys m2 r)
    | otherwise = let (xs, y, ys) = splitNodesAt (i - szf - szm) r
                  in (Br i f m xs, y, foldr cons Empty ys)
    where
      szf = sum $ map size f
      szm = size m

splitNodesAt 0 [x] = ([], x, [])
splitNodesAt i (x:xs) | i < size x = ([], x, xs)
                      | otherwise = let (xs', y, ys) = splitNodesAt (i - size x) xs
                                    in (x:xs', y, ys)

getAt :: Seq a -> Int -> a
getAt (Seq xs) i = getElem x where (_, x, _) = splitAt' i xs

extractAt :: Seq a -> Int -> (a, Seq a)
extractAt (Seq xs) i = let (l, x, r) = splitAt' i xs in (getElem x, Seq l +++ Seq r)

setAt :: Seq a -> Int -> a -> Seq a
setAt (Seq xs) i x = let (l, _, r) = splitAt' i xs in Seq l +++ (x <| Seq r)

-- move the i-th element to front
moveToFront :: Seq a -> Int -> Seq a
moveToFront xs i = let (a, xs') = extractAt xs i in a <| xs'

fromList :: [a] -> Seq a
fromList = foldr (<|) (Seq Empty)

toList :: Seq a -> [a]
toList (Seq Empty) = []
toList xs = (head' xs) : (toList $ tail' xs)

-- testing

prop_cons :: [Int] -> Bool
prop_cons xs = xs == (toList $ fromList xs)

prop_snoc :: [Int] -> Bool
prop_snoc xs = xs == (toList' $ foldl (|>) (Seq Empty) xs) where
    toList' (Seq Empty) = []
    toList' s = (toList' $ init' s) ++ [last' s]

prop_concat :: [Int] -> [Int] -> Bool
prop_concat xs ys = (xs ++ ys) == (toList ((fromList xs) +++ (fromList ys)))

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
-- [1]. Ralf Hinze and Ross Paterson. ``Finger Trees: A Simple General-purpose Data Structure,'' in Journal of Functional Programming 16:2 (2006), pages 197-217. http://www.soi.city.ac.uk/~ross/papers/FingerTree.html

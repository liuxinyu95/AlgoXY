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
            deriving (Show, Eq)

-- Finger Tree
data Tree a = Empty
            | Lf a
            | Br Int [a] (Tree (Node a)) [a] -- size, front, mid, rear
            deriving (Show, Eq)

newtype Elem a = Elem { getElem :: a } deriving (Show, Eq)

newtype Seq a = Seq (Tree (Elem a)) deriving (Show, Eq)

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

instance Sized (Seq a) where
  size (Seq xs) = size xs

tr2 a b = Tr2 (size a + size b) a b
tr3 a b c = Tr3 (size a + size b + size c) a b c

nodesOf (Tr2 _ a b) = [a, b]
nodesOf (Tr3 _ a b c) = [a, b, c]

-- Add from left
x <| Seq xs = Seq (Elem x `cons` xs)

cons :: (Sized a) => a -> Tree a -> Tree a
cons a Empty = Lf a
cons a (Lf b) = Br (size a + size b) [a] Empty [b]
cons a (Br s [b, c, d, e] m r) = Br (s + size a) [a, b] ((tr3 c d e) `cons` m) r
cons a (Br s f m r) = Br (s + size a) (a:f) m r

-- Remove from left
head' (Seq xs) = getElem $ fst $ uncons xs
tail' (Seq xs) = Seq $ snd $ uncons xs

uncons :: (Sized a) => Tree a -> (a, Tree a)
uncons (Lf a) = (a, Empty)
uncons (Br _ [a] Empty [b]) = (a, Lf b)
uncons (Br s [a] Empty (r:rs)) = (a, Br (s - size a) [r] Empty rs)
uncons (Br s [a] m r) = (a, Br (s - size a) (nodesOf f) m' r) where (f, m') = uncons m
uncons (Br s (a:f) m r) = (a, Br (s - size a) f m r)

-- Add from right
Seq xs |> x  = Seq (xs `snoc` Elem x)

snoc :: (Sized a) => Tree a -> a -> Tree a
snoc Empty a = Lf a
snoc (Lf a) b = Br (size a + size b) [a] Empty [b]
snoc (Br s f m [a, b, c, d]) e = Br (s + size e) f (m `snoc` (tr3 a b c)) [d, e]
snoc (Br s f m r) a = Br (s + size a) f m (r ++ [a])

-- Remove from right
last' (Seq xs) = getElem $ snd $ unsnoc xs
init' (Seq xs) = Seq $ fst $ unsnoc xs

unsnoc :: (Sized a) => Tree a -> (Tree a, a)
unsnoc (Lf a) = (Empty, a)
unsnoc (Br _ [a] Empty [b]) = (Lf a, b)
unsnoc (Br s f@(_:_:_) Empty [a]) = (Br (s - size a) (init f) Empty [last f], a)
unsnoc (Br s f m [a]) = (Br (s - size a) f m' (nodesOf r), a) where (m', r) = unsnoc m
unsnoc (Br s f m r) = (Br (s - size a) f m (init r), a) where a = last r

-- Concatenate
Seq xs +++ Seq ys = Seq (xs >+< ys)

xs >+< ys = merge xs [] ys

t <<< xs = foldl snoc t xs
xs >>> t = foldr cons t xs

merge :: (Sized a) => Tree a -> [a] -> Tree a -> Tree a
merge Empty es t2 = es >>> t2
merge t1 es Empty = t1 <<< es
merge (Lf a) es t2 = merge Empty (a:es) t2
merge t1 es (Lf a) = merge t1 (es++[a]) Empty
merge (Br s1 f1 m1 r1) es (Br s2 f2 m2 r2) =
    Br (s1 + s2 + (sum $ map size es)) f1 (merge m1 (trees (r1 ++ es ++ f2)) m2) r2

trees [a, b] = [tr2 a b]
trees [a, b, c] = [tr3 a b c]
trees [a, b, c, d] = [tr2 a b, tr2 c d]
trees (a:b:c:es) = (tr3 a b c):trees es

-- Index
data Place a = Place Int a deriving (Show, Eq)

getAt :: Seq a -> Int -> Maybe a
getAt (Seq xs) i | i < size xs = case lookupTree i xs of
                     Place _ (Elem x) -> Just x
                 | otherwise = Nothing

lookupTree :: (Sized a) => Int -> Tree a -> Place a
lookupTree _ Empty = error "lookup of empty tree"
lookupTree n (Lf a) = Place n a
lookupTree n (Br s f m r) | n < sf = lookups n f
                          | n < sm = case lookupTree (n - sf) m of
                                            Place n' xs -> lookupNode n' xs
                          | n < s = lookups (n - sm) r
                          | otherwise = error "out of bound"
  where sf = sum $ map size f
        sm = sf + size m

lookupNode :: (Sized a) => Int -> Node a -> Place a
lookupNode n (Tr2 _ a b) | n < sa = Place n a
                         | otherwise = Place (n - sa) b
  where sa = size a
lookupNode n (Tr3 _ a b c) | n < sa = Place n a
                           | n < sab = Place (n - sa) b
                           | otherwise = Place (n - sab) c
  where sa = size a
        sab = sa + size b

lookups :: (Sized a) => Int -> [a] -> Place a
lookups _ [] = error "empty list"
lookups n (x:xs) = if n < sx then Place n x
                   else lookups (n - sx) xs
  where sx = size x

-- | Cut
-- | Inspired by Dedekind cut, we cut *on* an element
-- |  unless the sequence is empty, or out of bound

cut :: Int -> Seq a -> (Seq a, Maybe a, Seq a)
cut i (Seq xs) | i < 0 = (Seq Empty, Nothing, Seq xs)
               | i < size xs = case cutTree i xs of
                 (a, Just (Place _ (Elem x)), b) -> (Seq a, Just x, Seq b)
                 (a, Nothing, b) -> (Seq a, Nothing, Seq b)
               | otherwise = (Seq xs, Nothing, Seq Empty)

cutTree :: (Sized a) => Int -> Tree a -> (Tree a, Maybe (Place a), Tree a)
cutTree _ Empty = (Empty, Nothing, Empty)
cutTree i (Lf a) | i < size a = (Empty, Just (Place i a), Empty)
                 | otherwise = (Lf a, Nothing, Empty)
cutTree i (Br s f m r)
  | i < sf = case cutList i f of
               (xs, x, ys) -> (Empty <<< xs, x, tree ys m r)
  | i < sm = case cutTree (i - sf) m of
               (t1, Just (Place i' a), t2) -> let (xs, x, ys) = cutNode i' a
                 in (tree f t1 xs, x, tree ys t2 r)
  | i < s  = case cutList (i - sm) r of
               (xs, x, ys) -> (tree f m xs, x, ys >>> Empty)
  | otherwise = error "cut tree: out of bound"
  where
    sf = sum $ map size f
    sm = sf + size m

cutList :: (Sized a) => Int -> [a] -> ([a], Maybe (Place a), [a])
cutList _ [] = ([], Nothing, [])
cutList i (x:xs) | i < sx = ([], Just (Place i x), xs)
             | otherwise = let (xs', y, ys) = cutList (i - sx) xs in (x:xs', y, ys)
  where sx = size x

cutNode :: (Sized a) => Int -> Node a -> ([a], Maybe (Place a), [a])
cutNode i (Tr2 _ a b) | i < sa = ([], Just (Place i a), [b])
                      | otherwise = ([a], Just (Place (i - sa) b), [])
  where sa = size a
cutNode i (Tr3 _ a b c) | i < sa = ([], Just (Place i a), [b, c])
                        | i < sab = ([a], Just (Place (i - sa) b), [c])
                        | otherwise = ([a, b], Just (Place (i - sab) c), [])
  where sa = size a
        sab = sa + size b

tree as Empty [] = as >>> Empty
tree [] Empty bs = Empty <<< bs
tree [] m r = Br (size m + sum (map size r)) (nodesOf f) m' r where (f, m') = uncons m
tree f m [] = Br (size m + sum (map size f)) f m' (nodesOf r) where (m', r) = unsnoc m
tree f m r = Br (size m + sum (map size f) + sum (map size r)) f m r

setAt s i x = case cut i s of
  (_, Nothing, _) -> s
  (xs, Just y, ys) -> xs +++ (x <| ys)

extractAt s i = case cut i s of (xs, Just y, ys) -> (y, xs +++ ys)

moveToFront s i = if i < 0 || i >= size s then s
                  else let (a, s') = extractAt s i in a <| s'

fromList :: [a] -> Seq a
fromList = foldr (<|) (Seq Empty)

toList :: Seq a -> [a]
toList (Seq Empty) = []
toList xs = (head' xs) : (toList $ tail' xs)

-- testing

prop_cons :: [Int] -> Bool
prop_cons xs = xs == (toList $ fromList xs)

prop_snoc :: [Int] -> Bool
prop_snoc xs = xs == (reverse $ reversed $ foldl (|>) (Seq Empty) xs) where
    reversed (Seq Empty) = []
    reversed s = (last' s) : (reversed $ init' s)

prop_concat :: [Int] -> [Int] -> Bool
prop_concat xs ys = (xs ++ ys) == (toList ((fromList xs) +++ (fromList ys)))

prop_lookup :: [Int] -> Bool
prop_lookup xs = and [Just (xs !! i) == getAt s i | i <- [0 .. length xs - 1]] where
  s = fromList xs

prop_cut :: [Int] -> Bool
prop_cut xs = and [(splitAt i xs) `eq` (cut i s) | i <- [-1 .. length xs]] where
   s = fromList xs
   eq (xs, ys) (s1, Nothing, s2) = xs == toList s1 && ys == toList s2
   eq (xs, ys) (s1, Just x, s2) = xs == toList s1 && ys == x : toList s2

prop_update :: [Int] -> Bool
prop_update xs = and [(updateAt i xs (i + 1)) == toList (setAt s i (i + 1))
                     | i <- [-1 .. length xs]] where
  s = fromList xs
  updateAt i xs x = if i < 0 then xs else case splitAt i xs of
    (as, _:bs) -> as ++ (x:bs)
    (as, []) -> xs

prop_mtf :: [Int] -> Bool
prop_mtf xs = and [(toList $ moveToFront s i) == mtf xs i | i <-[-1 .. length xs]]
    where
      s = fromList xs
      mtf xs i = if i < 0 || i >= length xs then xs
        else let (as, (b:bs)) = splitAt i xs in b : as ++ bs

testAll = do
  quickCheck prop_cons
  quickCheck prop_snoc
  quickCheck prop_concat
  quickCheck prop_lookup
  quickCheck prop_cut
  quickCheck prop_update
  quickCheck prop_mtf

-- Reference
-- [1]. Ralf Hinze and Ross Paterson. ``Finger Trees: A Simple General-purpose Data Structure,'' in Journal of Functional Programming 16:2 (2006), pages 197-217. http://www.soi.city.ac.uk/~ross/papers/FingerTree.html

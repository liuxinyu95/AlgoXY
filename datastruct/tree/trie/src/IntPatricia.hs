{-
    IntPatricia.hs, Integer base prefix tree.
    Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

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

-- Int (as key) Prefix Tree
-- Referred from Haskell packages Data.IntMap
-- Other reference includes:
--  [1] CLRS, Problems 12-2: Radix trees
--  [2] Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--	Workshop on ML, September 1998, pages 77-86,
--	<http://www.cse.ogi.edu/~andy/pub/finite.htm>

module IntPatricia where

import Data.Bits
import Test.QuickCheck hiding ((.&.))
import Data.Maybe (isNothing)
import Prelude hiding (lookup)

{------------------------------------
  1. Big Edian integer tree
-------------------------------------}
data IntTree a = Empty
               | Leaf Key a
               | Branch Prefix Mask (IntTree a) (IntTree a) -- prefix, mask, left, right

type Key = Int
type Prefix = Int
type Mask = Int

{-----------------------------------
  2. helpers
-----------------------------------}

-- join 2 nodes together.
-- (prefix1, tree1) ++ (prefix2, tree2)
--  1. find the longest common prefix == common(prefix1, prefix2)
--         prefix1 = a(n),a(n-1),...a(i+1),a(i),x...
--         prefix2 = a(n),a(n-1),...a(i+1),a(i),y...
--         prefix  = a(n),a(n-1),...a(i+1),a(i),00...0
--  2. mask bit = 100...0b (=2^i)
--         so mask is something like, 1,2,4,...,128,256,...
--  3. if      x=='0', y=='1' then (tree1=>left, tree2=>right),
--     else if x=='1', y=='0' then (tree2=>left, tree1=>right).
join :: Prefix -> IntTree a -> Prefix -> IntTree a -> IntTree a
join p1 t1 p2 t2 = if zero p1 m then Branch p m t1 t2
                                else Branch p m t2 t1
    where
      (p, m) = lcp p1 p2

-- 'lcp' means 'longest common prefix'
lcp :: Prefix -> Prefix -> (Prefix, Mask)
lcp p1 p2 = (p, m) where
    m = bit $ highestBit (p1 `xor` p2)
    p = mask p1 m

-- get the order of highest bit of 1.
-- For a number x = 00...0,1,a(i-1)...a(1)
-- the result is i
highestBit :: Int -> Int
highestBit x = if x==0 then 0 else 1 + highestBit (shiftR x 1)

-- For a number x = a(n),a(n-1)...a(i),a(i-1),...,a(0)
-- and a mask m = 100..0 (=2^i)
-- the result is a(n),a(n-1)...a(i),00..0
mask :: Int -> Mask -> Int
mask x m = (x .&. complement (m - 1)) -- complement means bitwise not.


-- Test if the next bit after mask bit is zero
-- For a number x = a(n),a(n-1)...a(i),1,...a(0)
-- and a mask   m = 100..0 (=2^i)
-- because the bit next to a(i) is 1, so the result is False
-- For a number y = a(n),a(n-1)...a(i),0,...a(0) the result is True.
zero :: Int -> Mask -> Bool
zero x m = x .&. (shiftR m 1) == 0

-- Test if a key matches a prefix above of the mask bit
-- For a prefix: p(n),p(n-1)...p(i)...p(0)
--     a key:    k(n),k(n-1)...k(i)...k(0)
-- and a mask:                 100..0 = (2^i)
-- If and only if p(j)==k(j), i<=j<=n the result is True
match :: Key -> Prefix -> Mask -> Bool
match k p m = (mask k m) == p

-- overwrite the previous value for duplicated key.
insert :: Key -> a -> IntTree a -> IntTree a
insert k x t
   = case t of
       Empty -> Leaf k x
       Leaf k' x' -> if k == k' then Leaf k x
                     else join k (Leaf k x) k' t
       Branch p m l r
          | match k p m -> if zero k m
                           then Branch p m (insert k x l) r
                           else Branch p m l (insert k x r)
          | otherwise -> join k (Leaf k x) p t

lookup :: Key -> IntTree a -> Maybe a
lookup _ Empty = Nothing
lookup k (Leaf k' v) = if k == k' then Just v else Nothing
lookup k (Branch p m l r) | match k p m = if zero k m then lookup k l else lookup k r
                          | otherwise = Nothing

-- Generate a Int tree from a list
-- Usage: fromList [(k1, x1), (k2, x2),..., (kn, xn)]
-- fromList :: [(Key, a)] -> IntTree a
fromList = foldr (uncurry insert) Empty

toString :: (Show a) => IntTree a -> String
toString t =
    case t of
      Empty -> "."
      Leaf k x -> (show k) ++ ":" ++ (show x)
      Branch p m l r -> "[" ++ (show p) ++ "@" ++ (show m) ++ "]" ++
                        "(" ++ (toString l) ++ ", " ++ (toString r) ++ ")"

{---------------------------------
  6. Test cases
----------------------------------}
testIntTree = "t=" ++ (toString t) ++ "\nlookup 4 t: " ++ (show $ lookup 4 t) ++
              "\nlookup 0 t: " ++ (show $ lookup 0 t)
    where
      t = fromList [(1, 'x'), (4, 'y'), (5, 'z')]

-- Verification

data Sample = S [(Key, Int)] [Int] deriving Show

instance Arbitrary Sample where
  arbitrary = do
    n <- choose (2, 100)
    xs <- shuffle [0..100]
    let (ks, ks') = splitAt n xs
    return $ S (zip ks [1..]) ks'

prop_build :: Sample -> Bool
prop_build (S kvs ks') = let t = fromList kvs in
  (all (\(k, v) -> Just v == lookup k t) kvs ) &&
  (all (isNothing . (flip lookup) t) ks')

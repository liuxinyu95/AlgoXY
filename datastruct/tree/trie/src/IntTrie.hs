{-
    IntTrie.hs, Integer base Trie
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

-- Int (as key) Radix tree
-- Referred from Haskell packages Data.IntMap
-- Other reference includes:
--  [1] CLRS, Problems 12-2: Radix trees
--  [2] Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--	Workshop on ML, September 1998, pages 77-86,
--	<http://www.cse.ogi.edu/~andy/pub/finite.htm>

-- A very simple int (binary) trie as CLRS 12-2 (Little-Edian)
module IntTrie where

import Test.QuickCheck
import Data.Maybe (isNothing)

data IntTrie a = Empty
               | Branch (IntTrie a) (Maybe a) (IntTrie a) -- left, value, right
               deriving Show

type Key = Int

-- accessors
left :: IntTrie a -> IntTrie a
left (Branch l _ _) = l
left Empty = Empty

right :: IntTrie a -> IntTrie a
right (Branch _ _ r) = r
right Empty = Empty

value :: IntTrie a -> Maybe a
value (Branch _ v _) = v
value Empty = Nothing

-- Insertion
-- override the value if key already exits
-- usage: insert trie key value
insert :: IntTrie a -> Key -> a -> IntTrie a
insert Empty k x = insert (Branch Empty Nothing Empty) k x
insert (Branch l v r) 0 x = Branch l (Just x) r
insert (Branch l v r) k x | even k    = Branch (insert l (k `div` 2) x) v r
                          | otherwise = Branch l v (insert r (k `div` 2) x)

-- Look up
search :: IntTrie a -> Key -> Maybe a
search Empty k = Nothing
search t 0 = value t
search t k = if even k then search (left t) (k `div` 2)
             else search (right t) (k `div` 2)

fromList :: [(Key, a)] -> IntTrie a
fromList xs = foldl ins Empty xs where
    ins t (k, v) = insert t k v

-- k = ... a2, a1, a0 ==> k' = ai * m + k, where m=2^i
toList :: IntTrie a -> [(Key, Maybe a)]
toList = toList' 0 1 where
  toList' _ _ Empty = []
  toList' k m (Branch l v r) = (toList' k (2 * m) l) ++
    ((k, v) : (toList' (m + k) (2 * m) r))

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
  (all (\(k, v) -> Just v == search t k) kvs ) &&
  (all (isNothing . search t) ks')

example = do
  let t = fromList [(1, 'a'), (4, 'b'), (5, 'c'), (9, 'd')]
  putStrLn $ show $ toList t
  putStrLn "search t 4"
  putStrLn $ show $ search t 4
  putStrLn "search t 0"
  putStrLn $ show $ search t 0

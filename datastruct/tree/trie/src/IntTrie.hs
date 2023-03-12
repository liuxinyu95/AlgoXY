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
import Prelude hiding (lookup)

data IntTrie a = Empty
               | Branch (IntTrie a) (Maybe a) (IntTrie a) -- left, value, right
               deriving Show

type Key = Int

left (Branch l _ _) = l
left Empty = Empty

right (Branch _ _ r) = r
right Empty = Empty

value (Branch _ v _) = v
value Empty = Nothing

-- override the value if key already exits
-- insert :: Key -> a -> IntTrie a -> IntTrie a
insert k x Empty = insert k x (Branch Empty Nothing Empty)
insert 0 x (Branch l v r) = Branch l (Just x) r
insert k x (Branch l v r) | even k    = Branch (insert (k `div` 2) x l) v r
                          | otherwise = Branch l v (insert (k `div` 2) x r)

lookup _ Empty = Nothing
lookup 0 (Branch _ v _) = v
lookup k (Branch l _ r) | even k    = lookup (k `div` 2) l
                        | otherwise = lookup (k `div` 2) r

fromList = foldr (uncurry insert) Empty

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
  (all (\(k, v) -> Just v == lookup k t) kvs ) &&
  (all (isNothing . (flip lookup) t) ks')

example = do
  let t = fromList [(1, 'a'), (4, 'b'), (5, 'c'), (9, 'd')]
  putStrLn $ show $ toList t
  putStrLn "lookup 4 t"
  putStrLn $ show $ lookup 4 t
  putStrLn "lookup 0 t"
  putStrLn $ show $ lookup 0 t

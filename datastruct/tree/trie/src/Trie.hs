{-
    Trie.hs, Alphabetic Trie.
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

-- Refer to http://en.wikipedia.org/wiki/Trie

module Trie (
  Trie, empty, isEmpty, insert, Trie.lookup, keys,
  AssocTrie(..), fromList, fromString
  ) where

import Data.List (sort, sortBy)
import Data.Function (on)
import qualified Data.Map as Map

class Trie t where
  empty :: t k v
  isEmpty :: t k v -> Bool
  insert :: (Eq k, Ord k) => [k] -> v -> t k v -> t k v
  lookup :: (Eq k, Ord k) => [k] -> t k v -> Maybe v
  keys :: (Eq k, Ord k) => t k v -> [[k]]

-- Default implementation: Assoc list based Trie
data AssocTrie k v = AssocTrie { value :: Maybe v
                               , subTrees :: [(k, AssocTrie k v)]} deriving (Show)

instance Trie AssocTrie where
  empty = AssocTrie Nothing []

  isEmpty (AssocTrie Nothing []) = True
  isEmpty _ = False

  insert [] x (AssocTrie _ ts) = AssocTrie (Just x) ts
  insert (k:ks) x (AssocTrie v ts) = AssocTrie v (ins ts) where
      ins [] = [(k, insert ks x empty)]
      ins ((c, t) : ts) = if c == k then (k, insert ks x t) : ts
                          else (c, t) : (ins ts)

  lookup [] t = value t
  lookup (k:ks) t = case Prelude.lookup k (subTrees t) of
                    Nothing -> Nothing
                    Just t' -> Trie.lookup ks t'

  -- Pre-order traverse to populate keys in lexicographical order
  keys t = map reverse $ keys' t [] where
    keys' t prefix = case (value t) of
      Nothing -> ks
      (Just _ ) -> prefix : ks
      where
        ks = concatMap (\(k, t') -> keys' t' (k : prefix)) ts
        ts = sortBy (compare `on` fst) (subTrees t)


fromList :: (Eq k, Ord k) => [([k], v)] -> AssocTrie k v
fromList = foldr (uncurry insert) empty

fromString :: String -> AssocTrie Char Integer
fromString = fromList . (flip zip [1..]) . words

-- example
example = insert "zoo" 0 (fromString "a place where animals are for public to see")

-- test data
assocs = [[("a", 1), ("an", 2), ("another", 7), ("boy", 3), ("bool", 4), ("zoo", 3)],
          [("zoo", 3), ("bool", 4), ("boy", 3), ("another", 7), ("an", 2), ("a", 1)]]

verify = all (\as ->
                 let t = fromList as in
                   all (\(k, v) -> maybe False (==v) (Trie.lookup k t)) as) assocs

verifyKeys = all (\as ->
                   keys (fromList as) == (sort $ fst $ unzip as)) assocs

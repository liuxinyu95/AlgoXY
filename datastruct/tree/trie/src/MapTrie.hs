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

module MapTrie (
  Trie, empty, isEmpty, insert, Trie.lookup, keys,
  MapTrie(..), fromList, fromString
  ) where

import Trie hiding (fromList, fromString)
import Data.List (sort, sortBy)
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (lookup)

-- Map based Trie
data MapTrie k v = MapTrie { value :: Maybe v
                           , subTrees :: Map.Map k (MapTrie k v)} deriving (Show)

instance Trie MapTrie where
  empty = MapTrie Nothing Map.empty

  isEmpty (MapTrie v ts) = isNothing v && Map.null ts

  insert [] x (MapTrie _ ts) = MapTrie (Just x) ts
  insert (k:ks) x (MapTrie v ts) = MapTrie v (Map.insert k (insert ks x t) ts)
    where
      t = maybe empty id (Map.lookup k ts)

  lookup [] (MapTrie v _) = v
  lookup (k:ks) (MapTrie _ ts) = case Map.lookup k ts of
                    Nothing -> Nothing
                    Just t' -> lookup ks t'

  keys t = map reverse $ keys' t [] where
    keys' (MapTrie v ts) prefix = case v of
      Nothing -> ks
      (Just _ ) -> prefix : ks
      where
        ks = concatMap (\(k, t') -> keys' t' (k : prefix)) (Map.toAscList ts)

fromList :: Ord k => [([k], v)] -> MapTrie k v
fromList = foldr (uncurry insert) empty

fromString :: String -> MapTrie Char Integer
fromString = fromList . (flip zip [1..]) . words

-- example
example = insert "zoo" 0 (fromString "a place where animals are for public to see")

-- test data
assocs = [[("a", 1), ("an", 2), ("another", 7), ("boy", 3), ("bool", 4), ("zoo", 3)],
          [("zoo", 3), ("bool", 4), ("boy", 3), ("another", 7), ("an", 2), ("a", 1)]]

verify = all (\as ->
                 let t = fromList as in
                   all (\(k, v) -> maybe False (==v) (lookup k t)) as) assocs

verifyKeys = all (\as ->
                   keys (fromList as) == (sort $ fst $ unzip as)) assocs

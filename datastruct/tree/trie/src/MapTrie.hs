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

module MapTrie where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (on)
import Control.Arrow (first)
import Prelude hiding (lookup)
import Data.List (isPrefixOf, sort, sortBy, inits, nub)

-- Map based Trie
data MapTrie k v = MapTrie { value :: Maybe v
                           , subTrees :: Map k (MapTrie k v)} deriving (Show)

empty = MapTrie Nothing Map.empty

insert [] x (MapTrie _ ts) = MapTrie (Just x) ts
insert (k:ks) x (MapTrie v ts) = MapTrie v (Map.insert k (insert ks x t) ts)
  where
    t = maybe empty id (Map.lookup k ts)

lookup [] (MapTrie v _) = v
lookup (k:ks) (MapTrie _ ts) = case Map.lookup k ts of
                  Nothing -> Nothing
                  Just t' -> lookup ks t'

-- Pre-order traverse to populate keys in lexicographical order
keys t = map reverse $ keys' t [] where
  keys' (MapTrie v ts) prefix = case v of
    Nothing -> ks
    (Just _ ) -> prefix : ks
    where
      ks = concatMap (\(k, t') -> keys' t' (k : prefix)) (Map.toAscList ts)

fromList = foldr (uncurry insert) empty

fromString = fromList . (flip zip [1..]) . words

-- Applications

startsWith :: Ord k => [k] -> MapTrie k v -> [([k], v)]
startsWith [] (MapTrie Nothing  ts) = enum ts
startsWith [] (MapTrie (Just v) ts) = ([], v) : enum ts
startsWith (k:ks) (MapTrie _ ts) = case Map.lookup k ts of
  Nothing -> []
  Just t -> map (first (k:)) (startsWith ks t)

enum :: Ord k => Map k (MapTrie k v) -> [([k], v)]
enum = (concatMap (\(k, t) ->
                     map (first (k:)) (startsWith [] t))) . Map.assocs

-- look up the trie up to n candidates
get n k t = take n $ startsWith k t

-- ITU-T keypad (T9) mapping: digit -> [char]
mapT9 = Map.fromList [('1', ",."), ('2', "abc"), ('3', "def"), ('4', "ghi"),
                      ('5', "jkl"), ('6', "mno"), ('7', "pqrs"), ('8', "tuv"),
                      ('9', "wxyz")]

-- reverse ITU-T keypad map: char -> digit
rmapT9 = Map.fromList $ concatMap (\(d, s) -> [(c, d) | c <- s]) $ Map.toList mapT9

digits = map (\c -> Map.findWithDefault '#' c rmapT9)

-- Given a list of digits, find all candidate words (including partial words)
-- with T9 map from a dictionary (implemented in prefix Tree).
findT9 [] _ = [[]]
findT9 (d:ds) (MapTrie _ ts) = concatMap find cts where
  cts = case Map.lookup d mapT9 of
    Nothing -> []
    Just cs -> Map.assocs $ Map.filterWithKey (\c _ -> c `elem` cs) ts
  find (c, t) = map (c:) (findT9 ds t)

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

verifyStartsWith = all verifyLookup [("a", 5), ("a", 6), ("a", 7), ("ab", 2),
                                     ("ab", 5), ("b", 2), ("bo", 5), ("z", 3)]
    where
      lst=[("a", "the first letter of English"),
           ("an", "used instead of 'a' when the following word begins with a vowel sound"),
           ("another", "one more person or thing or an extra amount"),
           ("abandon", "to leave a place, thing or person forever"),
           ("about", "on the subject of; connected with"),
           ("adam", "a character in the Bible who was the first man made by God"),
           ("boy", "a male child or, more generally, a male of any age"),
           ("bodyl", "the whole physical structure that forms a person or animal"),
           ("zoo", "an area in which animals, especially wild animals, are kept so that people can go and look at them, or study them")]
      t = fromList lst
      m = Map.fromList lst
      verifyLookup (k, n) = length r <= n &&
                            all (\(k', v) -> k `isPrefixOf` k' && k' `Map.member` m) r
        where
          r = get n k t

verifyT9 = all verify' $ concatMap (tail . inits) ["4663", "22", "2668437"]
  where
    t9lst = zip ["home", "good", "gone", "hood", "a", "another", "an"] [1..]
    verify' ds = ((==) `on` sort . nub) as bs where
      as = findT9 ds (fromList t9lst)
      bs = filter ((==) ds . digits) (map (take (length ds) . fst) t9lst)

verifyAll = and [verify, verifyKeys, verifyStartsWith, verifyT9]

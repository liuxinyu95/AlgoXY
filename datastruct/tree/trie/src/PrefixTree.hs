{-
    PrefixTree.hs, Alphabetic Prefix Tree.
    Although it's called 'alphabetic', we abstract the key as sequence
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

module PrefixTree where

import Prelude hiding (lookup)
import Data.List (isPrefixOf, sort, sortBy, inits, nub, find)
import Data.Function (on)
import Control.Arrow (first)
import qualified Data.Map as Map

-- definition
data PrefixTree k v = PrefixTree { value :: Maybe v
                                 , subTrees :: [([k], PrefixTree k v)]}
                      deriving Show

empty = PrefixTree Nothing []

leaf v = PrefixTree (Just v) []

insert (PrefixTree _ ts) [] v = PrefixTree (Just v) ts
insert (PrefixTree v' ts) k v = PrefixTree v' (ins ts) where
    ins [] = [(k, leaf v)]
    ins ((k', t) : ts) | match k k' = (branch k v k' t) : ts
                       | otherwise  = (k', t) : ins ts

match [] _ = True
match _ [] = True
match (a:_) (b:_) = a == b

branch a v b t = case lcp a b of
  (c, [], b') -> (c, PrefixTree (Just v) [(b', t)])
  (c, a', []) -> (c, insert t a' v)
  (c, a', b') -> (c, PrefixTree Nothing [(a', leaf v), (b', t)])

-- longest common prefix
lcp [] bs = ([], [], bs)
lcp as [] = ([], as, [])
lcp (a:as) (b:bs) | a /= b = ([], a:as, b:bs)
                  | otherwise = (a:cs, as', bs') where (cs, as', bs') = lcp as bs

-- lookup
lookup [] (PrefixTree v _) = v
lookup ks (PrefixTree v ts) = case find (\(s, t) -> s `isPrefixOf` ks) ts of
  Nothing -> Nothing
  Just (s, t) -> lookup (drop (length s) ks) t

fromList :: Eq k => [([k], v)] -> PrefixTree k v
fromList = foldl ins empty where
    ins t (k, v) = insert t k v

fromString :: (Enum v, Num v) => String -> PrefixTree Char v
fromString = fromList . (flip zip [1..]) . words

-- Pre-order traverse to populate keys in lexicographical order.
keys :: Ord k => PrefixTree k v -> [[k]]
keys = keys' [] where
  keys' prefix t = case (value t) of
    Nothing -> kss
    (Just _) -> prefix : kss
    where
      kss = concatMap (\(ks, t') -> keys' (prefix ++ ks) t') ts
      ts = sortBy (compare `on` fst) (subTrees t)

startsWith [] (PrefixTree Nothing ts) = enum ts
startsWith [] (PrefixTree (Just v) ts) = ([], v) : enum ts
startsWith k (PrefixTree _ ts) =
  case find (\(s, t) -> s `isPrefixOf` k || k `isPrefixOf` s) ts of
    Nothing -> []
    Just (s, t) -> map (first (s ++)) (startsWith (drop (length s) k) t)

enum = concatMap (\(k, t) -> map (first (k ++)) (startsWith [] t))

-- T9 (Textonym) lookup
mapT9 = Map.fromList [('1', ",."), ('2', "abc"), ('3', "def"), ('4', "ghi"),
                      ('5', "jkl"), ('6', "mno"), ('7', "pqrs"), ('8', "tuv"),
                      ('9', "wxyz")]

-- reverse T9 map
rmapT9 = Map.fromList $ concatMap (\(d, s) -> [(c, d) | c <- s]) $ Map.toList mapT9

digits = map (\c -> Map.findWithDefault '#' c rmapT9)

findT9 :: PrefixTree Char v -> String -> [String]
findT9 t [] = [""]
findT9 t k = concatMap find prefixes
  where
    n = length k
    find (s, t') = map (take n . (s++)) $ findT9 t' (k `diff` s)
    diff x y = drop (length y) x
    prefixes = [(s, t') | (s, t') <- subTrees t, let ds = digits s in
                          ds `isPrefixOf` k || k `isPrefixOf` ds]

-- look up the prefix tree up to n candidates
get n t k = take n $ startsWith k t

--
example = insert (fromString "a place where animals are for public to see") "zoo" 0

-- test data
assocs = [[("a", 1), ("an", 2), ("another", 7), ("boy", 3), ("bool", 4), ("zoo", 3)],
          [("zoo", 3), ("bool", 4), ("boy", 3), ("another", 7), ("an", 2), ("a", 1)]]

-- tests
verify = all (\as ->
                 let t = fromList as in
                   all (\(k, v) -> maybe False (==v) (lookup k t)) as) assocs

verifyKeys = all (\as ->
                   keys (fromList as) == (sort $ fst $ unzip as)) assocs

verifyFindAll = all verifyLookup [("a", 5), ("a", 6), ("a", 7), ("ab", 2),
                                  ("ab", 5), ("b", 2), ("bo", 5), ("z", 3)]
  where
    lst = [("a", "the first letter of English"),
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
        r = get n t k

verifyT9 = all verify' $ concatMap (tail . inits) ["4663", "22", "2668437"]
  where
    t9lst = [("home", 1), ("good", 2), ("gone", 3), ("hood", 4), ("a", 5), ("another", 6), ("an", 7)]
    verify' ds = ((==) `on` sort . nub) as bs where
      as = findT9 (fromList t9lst) ds
      bs = filter ((==) ds . digits) (map (take (length ds) . fst) t9lst)

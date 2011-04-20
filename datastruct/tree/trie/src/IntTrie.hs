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

data IntTrie a = Empty 
               | Branch (IntTrie a) (Maybe a) (IntTrie a) -- left, value, right

type Key = Int

-- helpers
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
-- if user insert a value already binding with existed key, just over write 
-- the previous value
-- usage: insert trie key x
insert :: IntTrie a -> Key -> a -> IntTrie a
insert t 0 x = Branch (left t) (Just x) (right t)
insert t k x = if even k
               then Branch (insert (left t) (k `div` 2) x) (value t) (right t)
               else Branch (left t) (value t) (insert (right t) (k `div` 2) x)

-- Look up
search :: IntTrie a -> Key -> Maybe a
search Empty k = Nothing
search t 0 = value t
search t k = if even k then search (left t) (k `div` 2)
             else search (right t) (k `div` 2)

-- Test helper
fromList :: [(Key, a)] -> IntTrie a
fromList xs = foldl ins Empty xs where
    ins t (k, v) = insert t k v

-- k = ... a2, a1, a0 ==> k' = ai * m + k, where m=2^i
toString :: (Show a)=>IntTrie a -> String
toString t = toStr t 0 1 where
    toStr Empty k m = "."
    toStr tr k m = "(" ++ (toStr (left tr) k (2*m)) ++
                      " " ++ (show k) ++ (valueStr (value tr)) ++
                      " " ++ (toStr (right tr) (m+k) (2*m)) ++ ")"
    valueStr (Just x) = ":" ++ (show x)
    valueStr _ = ""

-- Test cases
testIntTrie = "t=" ++ (toString t) ++ "\nsearch t 4: " ++ (show $ search t 4) ++
              "\nsearch t 0: " ++ (show $ search t 0)
    where
      t = fromList [(1, 'a'), (4, 'b'), (5, 'c'), (9, 'd')]


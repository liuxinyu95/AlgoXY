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
module Trie where

-- definition
data Trie a = Trie { value :: Maybe a
                   , children :: [(Char, Trie a)]}

empty = Trie Nothing []

-- insert
-- usage: insert t "zoo" "a place where animals are for public to see"
insert :: Trie a -> String -> a -> Trie a
insert t []     x = Trie (Just x)  (children t)
insert t (k:ks) x = Trie (value t) (ins (children t) k ks x) where
    ins [] k ks x = [(k, (insert empty ks x))]
    ins (p:ps) k ks x = if fst p == k 
                        then (k, insert (snd p) ks x):ps
                        else p:(ins ps k ks x)

-- lookup
find :: Trie a -> String -> Maybe a
find t [] = value t
find t (k:ks) = case lookup k (children t) of
                  Nothing -> Nothing
                  Just t' -> find t' ks

fromList :: [(String, a)] -> Trie a
fromList xs = foldl ins empty xs where
    ins t (k, v) = insert t k v

sort :: (Ord a)=>[(a, b)] -> [(a, b)]
sort [] = []
sort (p:ps) = sort xs ++ [p] ++ sort ys where
    xs = [x | x<-ps, fst x <= fst p ]
    ys = [y | y<-ps, fst y > fst p ]

toString :: (Show a)=> Trie a -> String
toString t = toStr t "" where
    toStr t prefix = "(" ++ prefix ++ showMaybe (value t) ++ 
                     (concat $ map (\(k, v)-> ", " ++ toStr v (prefix++[k])) 
                                 (sort $ children t))
                     ++ ")"
    showMaybe Nothing = ""
    showMaybe (Just x)  = ":" ++ show x

-- test cases

testTrie = "t=" ++ (toString t) ++ 
           "\nt'=" ++ (toString t') ++
           "\nsearch t an: " ++ (show $ find t "an") ++
           "\nsearch t boy: " ++ (show $ find t "boy") ++
           "\nsearch t the: " ++ (show $ find t "the")
    where 
      t = fromList [("a", 1), ("an", 2), ("another", 7), ("boy", 3), ("bool", 4), ("zoo", 3)]
      t'= fromList [("zoo", 3), ("bool", 4), ("boy", 3), ("another", 7), ("an", 2), ("a", 1)]


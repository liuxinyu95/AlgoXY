{-
    STree.hs, Suffix Tree/Trie in Haskell.
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

module STree where

import Data.List

-- According to:
--   * Robert Giegerich and Stefan Kurtz, \"/A comparison of
--     imperative and purely functional suffix tree constructions/\",
--     Science of Computer Programming 25(2-3):187-218, 1995,
--     <http://citeseer.ist.psu.edu/giegerich95comparison.html>
--
-- Liu Xinyu: This method is a kind of "Brute Force" algorithm in fact.
--  It can't match O(n), n=length(word), as Ukkonen, McCreight and Weiner's.
--  Altough it can benifit from Haskell's lazy evaluation, so that
--  without traverse, the tree won't be constructed.
--
--  However, in other functional programming language, such as ML, Scheme/Lisp
--  Which doesn't do lazy evaulation by default, this method
--  is equivalent to:
--      foldl patricia-insert empty tails(word)
--

-- Liu Xinyu: pure brute-force method with list comprehension
--
data Tr = Lf | Br [(String, Tr)] deriving (Eq, Show)
type EdgeFunc = [String]->(String, [String])

alpha = ['a'..'z']++['A'..'Z']++['#', '$']

-- Create Lazy Radix Tree
-- some helper comments
--  xs@(x:_)<-[...], The @(x:_) is used to filter out the empty elements
lazyTree::EdgeFunc -> [String] -> Tr
lazyTree edge = build where
    build [[]] = Lf
    build ss = Br [(a:prefix, build ss') | a<-alpha, 
                                           xs@(x:_) <-[[cs | c:cs<-ss, c==a]],
                                           (prefix, ss')<-[edge xs]]

-- Trie
-- ast: Atomic Suffix Tree,  means Suffix Trie
edgeTrie::EdgeFunc
edgeTrie ss = ("", ss)

-- Patricia: Extract the longest common prefix
-- cst: Compact Suffix Tree, Suffix Patricia
-- ex: 
--   edgeTree ["an", "another", "and"] = ("an", ["", "other", "d"])
--   edgeTree ["bool", "foo", "bar"] = ("", ["bool", "foo", "bar"])
--
-- some helper comments
--   let awss@((a:w):ss) = ["an", "another", "and"]
--       (a:w) = "an",  ss = ["another", "and"]
--       a='a', w="n"
--       rests awss = w:[u| _:u<-ss] = ["n", "nother", "nd"]
--
edgeTree::EdgeFunc
edgeTree [s] = (s, [[]])
edgeTree awss@((a:w):ss) | null [c|c:_<-ss, a/=c] = (a:prefix, ss')
                         | otherwise              = ("", awss)
                         where (prefix, ss') = edgeTree (w:[u| _:u<-ss])
edgeTree ss = ("", ss) -- (a:w):ss can't be match <==> head ss == ""

suffixTrie::String->Tr
suffixTrie = lazyTree edgeTrie . tails -- or init . tails

suffixTree::String->Tr
suffixTree = lazyTree edgeTree . tails

-- Let's prove it's brute-force
trie::[String]->Tr
trie = lazyTree edgeTrie

patricia::[String]->Tr
patricia = lazyTree edgeTree

-- testing

testSuffixTrie s = "SuffixTrie(\"" ++ s ++ "\")=" ++ (show $ suffixTrie s) ++ "\n"

testSuffixTree s = "SuffixTree(\"" ++ s ++ "\")=" ++ (show $ suffixTree s) ++ "\n"

testTrie ss = "Trie(\"" ++ (show ss) ++ "\")=" ++ (show $ trie ss) ++ "\n"

testPatricia ss = "Patricia(\"" ++ (show ss) ++ "\")=" ++ (show $ patricia ss) ++ "\n"

test = concat [ f s | s <- ["cacao", "mississippi", "banans"],
                      f <- [testSuffixTrie, testSuffixTree]] ++
       testTrie ["zoo", "bool", "boy", "another", "an", "a"] ++
       testPatricia ["zoo", "bool", "boy", "another", "an", "a"]
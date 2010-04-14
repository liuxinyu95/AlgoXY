{-
    Patricia.hs, Alphabetic Patricia Tree.
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

-- method 1
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
data STree = Leaf | Branch [(Edge, STree)] deriving (Eq, Show)
type Edge = (String, Int)
type EdgeFunction = [String]->(Int, [String])

alpha = ['a'..'z']++['A'..'Z']

lazyTree::EdgeFunction->[String]->STree
lazyTree edge = stree where
    stree [[]] = Leaf
    stree ss = Branch [((a:sa, 1+cpl), stree ss') 
                           | a<-alpha,
                             x@(sa:_) <- [ss `selectBy` a],
                             (cpl, ss') <- [edge x]]
    selectBy ss a = [cs | c:cs <-ss, c==a] -- select suffixes starting with a

--ast: Atomic Suffix Tree,  means Suffix Trie
ast::EdgeFunction
ast ss = (0, ss)

--cst: Compact Suffix Tree, Suffix Patricia
--     extract the longest common prefix
cst::EdgeFunction
cst [s] = (length s, [[]])
cst awss@((a:w):ss) | null [c | c:_<-ss, a /= c] = (1+cpl, ss')
                    | otherwise                  = (0, awss)
                    where (cpl, ss') = cst (w:[u | _:u<-ss])

suffixes = init.tails -- returns non-empty suffixes

suffixTrie::String->STree
suffixTrie = lazyTree ast . suffixes

suffixTree::String->STree
suffixTree = lazyTree cst . suffixes

-- method 2
-- Liu Xinyu: pure brute-force method with list comprehension
--
data Tr = Lf | Br [(String, Tr)] deriving (Eq, Show)
type EdgeFunc = [String]->(String, [String])

-- Create Lazy Radix Tree
-- some helper comments
--  xs@(x:_)<-[...], The @(x:_) is used to filter out the empty elements
lazyTr::EdgeFunc -> [String] -> Tr
lazyTr edge = build where
    build [[]] = Lf
    build ss = Br [(a:prefix, build ss') | a<-alpha, 
                                           xs@(x:_) <-[[cs | c:cs<-ss, c==a]],
                                           (prefix, ss')<-[edge xs]]

-- Trie
edgeTrie::EdgeFunc
edgeTrie ss = ("", ss)

-- Patricia: Extract the longest common prefix
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

sufTrie::String->Tr
sufTrie = lazyTr edgeTrie . tails

sufTree::String->Tr
sufTree = lazyTr edgeTree . tails

-- Let's prove it's brute-force
trie::[String]->Tr
trie = lazyTr edgeTrie

patricia::[String]->Tr
patricia = lazyTr edgeTree

-- Method 3
-- Liu Xinyu: Construct the suffix tree from left to right
update::Tr->Char->Tr
update Lf c = Br [([c], Lf)]
update (Br ts) c = case find (\(s, _)->c==head s) ts of 
                   Nothing -> Br ((map (insertAfter c c') ts)++[([c], Lf)])
                   _       -> Br ((map (insertAfter c c') ts)) --Br (map (appendLeaf c) ts)
    where
      c' = lastElem $ head ts -- c' is the last char of any edge to leaf
                                      
appendLeaf::Char->(String, Tr)->(String, Tr)
appendLeaf c (s, Lf) = (s++[c], Lf)
appendLeaf c (s, (Br ts)) = (s, Br (map (appendLeaf c) ts))
                   
lastElem::(String, Tr)->Char
lastElem (s, Lf) = last s
lastElem (_, Br ts) = lastElem $ head ts

--(s1c's2c'...snc's, t) --> (s1c', Br [(c, Lf), (s2c', Br [(c, Lf), Br ...
insertAfter::Char->Char->(String, Tr)->(String, Tr)
insertAfter c c' (s, t) = ins c lst where
	lst = split (c'==) s
        ins c [x] = if isLeaf t then (x++[c], t) else (x, update t c)
	ins c (x:xs) = (x, Br [(ins c xs), ([c], Lf)])

isLeaf::Tr -> Bool
isLeaf Lf = True
isLeaf _  = False

split _ [] = []
split f lst | ys == [] = [xs] 
            | otherwise = (xs++[head ys]): split f (tail ys)
    where (xs, ys) = break f lst

suffixTree'::String->Tr
suffixTree' = foldl update Lf

-- testing

testSuffixTrie s = "Robert, Kurtz:\n SuffixTrie(\"" ++ s ++ "\")=" ++ (show $ suffixTrie s) ++ "\n"

testSuffixTree s = "Robert, Kurtz:\n SuffixTree(\"" ++ s ++ "\")=" ++ (show $ suffixTree s) ++ "\n"

testSuffixTrie' s = "Liu Xinyu:\n SuffixTrie(\"" ++ s ++ "\")=" ++ (show $ sufTrie s) ++ "\n"

testSuffixTree' s = "Liu Xinyu:\n SuffixTree(\"" ++ s ++ "\")=" ++ (show $ sufTree s) ++ "\n"

testTrie ss = "Trie(\"" ++ (show ss) ++ "\")=" ++ (show $ trie ss) ++ "\n"

testPatricia ss = "Patricia(\"" ++ (show ss) ++ "\")=" ++ (show $ patricia ss) ++ "\n"

test = concat [ f s | s <- ["cacao", "mississippi", "banans"],
                      f <- [testSuffixTrie, testSuffixTree, testSuffixTrie', testSuffixTree']] ++
       testTrie ["zoo", "bool", "boy", "another", "an", "a"] ++
       testPatricia ["zoo", "bool", "boy", "another", "an", "a"]
{-
    STreeStr.hs, String manipulation with Suffix Tree in Haskell.
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

module STreeStr where

import STree
import Data.Function (on)
import Data.List (maximumBy, isInfixOf, isPrefixOf)

-- Different from Data.List.maximumBy, returns all biggest elements
maxBy::(Ord a)=>(a->a->Ordering)->[a]->[a]
maxBy _ [] = []
maxBy cmp (x:xs) = foldl maxBy' [x] xs where
    maxBy' lst y = case cmp (head lst) y of
                     GT -> lst
                     EQ -> lst ++ [y]
                     LT -> [y]

isLeaf::Tr -> Bool
isLeaf Lf = True
isLeaf _ = False

-- find the ocurrence of a pattern
lookupPattern :: Tr -> String -> Int
lookupPattern (Br lst) ptn = find lst where
    find [] = 0
    find ((s, t):xs)
         | ptn `isPrefixOf` s = numberOfBranch t
         | s `isPrefixOf` ptn = lookupPattern t (drop (length s) ptn)
         | otherwise = find xs
    numberOfBranch (Br ys) = length ys
    numberOfBranch _ = 1

findPattern :: String -> String -> Int
findPattern s ptn = lookupPattern (suffixTree $ s++"$") ptn

-- Search the longest repeated substringS
--  returns all results if there are multiple repeated substr with same length
lrs::Tr->[String]
lrs Lf = [""]
lrs (Br lst) = find $ filter (not . isLeaf . snd) lst where
    find [] = [""]
    find ((s, t):xs) = maxBy (compare `on` length) 
                             ((map (s++) (lrs t)) ++ (find xs))

longestRepeatedSubstrings::String->[String]
longestRepeatedSubstrings s = lrs $ suffixTree (s++"$")

-- Search the longest repeated substring
--  Only return the first result
lrs'::Tr->String
lrs' Lf = ""
lrs' (Br lst) = find $ filter (not . isLeaf . snd) lst where
    find [] = ""
    find ((s, t):xs) = maximumBy (compare `on` length) [s++(lrs' t), find xs]

longestRepeatedSubstring :: String -> String
longestRepeatedSubstring s = lrs' $ suffixTree (s++"$")

-- Search the longest common substringS
lcs::Tr->[String]
lcs Lf = []
lcs (Br lst) = find $ filter (not .isLeaf . snd) lst where
    find [] = []
    find ((s, t):xs) = maxBy (compare `on` length) 
                       (if match t 
                        then s:(find xs)
                        else  (map (s++) (lcs t)) ++ (find xs))

-- Search the longest common substring
lcs'::Tr->String
lcs' Lf = ""
lcs' (Br lst) = find $ filter (not . isLeaf . snd) lst where
    find [] = ""
    find ((s, t):xs) = maximumBy (compare `on` length)
                       (if match t then [s, find xs]
                        else [tryAdd s (lcs' t), find xs])
    tryAdd x y = if y=="" then "" else x++y

match (Br [(s1, Lf), (s2, Lf)]) = ("#" `isInfixOf` s1) /= ("#" `isInfixOf` s2)
match _ = False

longestCommonSubstrings s1 s2 = lcs $ suffixTree (s1++"#"++s2++"$")
longestCommonSubstring s1 s2 = lcs' $ suffixTree (s1++"#"++s2++"$")

longestPalindromes s = lcs $ suffixTree (s++"#"++(reverse s)++"$")

longestPalindrome s = lcs' $ suffixTree (s++"#"++(reverse s)++"$")

-- tests
testPattern = ["find pattern "++p++" in banana: "++
               (show $ findPattern "banana" p)
                   | p<- ["ana", "an", "anan", "nana", "anana"]]

testLRS s = "LRS(" ++ s ++ ")=" ++ (show $ lrs $ suffixTree (s++"$")) ++ "\n"

testLRS' s = "LRS'(" ++ s ++ ")=" ++ (lrs' $ suffixTree (s++"$")) ++ "\n"

testLCS s1 s2 = "LCS(" ++ s1 ++", "++ s2 ++")=" ++ (show $ lcs $ suffixTree (s1++"#"++s2++"$"))++"\n"

testPalindrome s = "longest palindrome(" ++ s ++ ")=" ++ (show $ longestPalindromes s) ++ "\n"

testMain = concat [ f s | s<-["mississippi", "banana", "cacao", "foofooxbarbar"],
                          f<-[testLRS, testLRS', testPalindrome]]
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

-- Search the longest repeated substring
lrs::Tr->[String]
lrs Lf = [""]
lrs (Br lst) = find $ filter (not . isLeaf . snd) lst where
    find [] = [""]
    find ((s, t):xs) = maxBy (compare `on` length) 
                             ((map (s++) (lrs t)) ++ (find xs))

testLRS s = "LRS(" ++ s ++ ")=" ++ (show $ lrs $ suffixTree (s++"$")) ++ "\n"


testMain = concat [ f s | s<-["mississippi", "banana", "cacao"],
                          f<-[testLRS]]
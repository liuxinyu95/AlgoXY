-- Occurrence.hs
-- Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Occurrence where

import qualified Data.Map as M
import Data.List (maximumBy, foldl1)
import Data.Function (on)
import Data.Maybe

-- problem 1: find the element occurs most
maxOccur xs = maximumBy (compare `on` snd) $ M.toList $ M.fromListWith (+) $ zip xs $ repeat 1

-- problem 2: find the longest sub-string contains same element.
longestDup (x:xs) = fst $ foldl f ((x, 1), (x, 1)) xs where
    f ((a, n), (a', n')) b | a' /=b = ((a, n), (b, 1))
                           | n < n'+1 = ((a', n'+1), (a', n'+1))
                           | otherwise = ((a, n), (a', n'+1))
-- Divide and conquer
-- problem 1
find [] = Nothing
find (x:xs) = Just $ maximumBy (compare `on` snd) $ (x, l):catMaybes 
                [find [ a| a<-xs, a < x], find [ a| a<-xs, a > x]]
  where
    l = length (x:[ a| a<-xs, a == x])

-- Boyer-Moore linear time majority vote algorithm

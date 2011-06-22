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

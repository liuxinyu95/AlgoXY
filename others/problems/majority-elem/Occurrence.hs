module Occurrence where

import qualified Data.Map as M
import Data.List (maximumBy, foldl1)
import Data.Function (on)

maxOccur xs = maximumBy (compare `on` snd) $ M.toList $ M.fromListWith (+) $ zip xs $ repeat 1

longestDup (x:xs) = fst $ foldl f ((x, 1), (x, 1)) xs where
    f ((a, n), (a', n')) b | a' /=b = ((a, n), (b, 1))
                           | n < n'+1 = ((a', n'+1), (a', n'+1))
                           | otherwise = ((a, n), (a', n'+1))

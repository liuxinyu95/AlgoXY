module Bar where

import Data.Function (on)
import Data.List

--solve :: (Ord a, Num a)=>[a] -> a -> [[a]]
solve as s | s == 0 = [[]]
           | otherwise = concat [ map (x:) (solve as (s-x)) |  
                                  x <- as, s-x >=0 ]

mapCons x [] = [[x]]
mapCons x ys = map (x:) ys


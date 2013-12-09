module Queens where

import Debug.Trace (trace)

solve = dfsSolve [[]] [] where
    dfsSolve [] s = s
    dfsSolve (c:cs) s
             | length c == 8 = dfsSolve cs (c:s)
             | otherwise = dfsSolve ([(x:c) | x <- [1..8], not $ attack x c] ++ cs) s
    attack x cs = let y = 1 + length cs in 
                 x `elem` cs || 
                 (any (\(c, r) -> abs(x - c) == abs(y - r)) $ zip (reverse cs) [1..])


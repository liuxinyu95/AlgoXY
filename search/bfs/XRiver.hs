module XRiver where

import Data.Bits -- for bitwise operation

-- wolf: 1, goat: 2, cabbage: 4, farmer: 8
-- conflict: wolf + goat = 3, goat + cabbage = 6;

-- Find the only solution or prove there is no solution.
solve = bfsSolve [[(15, 0)]] where
    bfsSolve :: [[(Int, Int)]] -> [(Int, Int)]
    bfsSolve [] = [] -- no solution
    bfsSolve (c:cs) | (fst $ head c) == 0 = reverse c
                    | otherwise = bfsSolve (cs ++ map (:c) 
                                      (filter (`valid` c) $ moves $ head c))

valid (a, b) r = not $ or [ a `elem` [3, 6], b `elem` [3, 6], (a, b) `elem` r]

moves (a, b) = if b < 8 then trans a b else map swap (trans b a) where
    trans x y = [(x - 8 - i, y + 8 + i) 
                     | i <-[0, 1, 2, 4], i == 0 || (x .&. i) /= 0]
    swap (x, y) = (y, x)

toWgc = unlines . map (\(a, b)-> wgc a ++ "----" ++ wgc b) where
    wgc x = show $ map snd $ filter (\(i, _) ->  x .&. i /= 0)
                         [(1, "wolf"), (2, "goat"), (4, "cabbage"), (8, "farmer")]

prt = putStrLn $ toWgc $ solve

-- find all solutions
solve' = bfsSolve [[(15, 0)]] where
    bfsSolve :: [[(Int, Int)]] -> [[(Int, Int)]]
    bfsSolve [] = []
    bfsSolve (c:cs) | (fst $ head c) == 0 = (reverse c) : bfsSolve cs
                    | otherwise = bfsSolve (cs ++ map (:c) 
                                      (filter (`valid` c) $ moves $ head c))

prtAll = putStrLn $ unlines (map toWgc solve')

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

moves (a, b) = if a .&. 8  /= 0 then trans a b else map swap (trans b a) where
    trans x y = (x - 8, y + 8):[(x .&. (7 - i), y .|. (8 + i)) 
                                    | i <-[1, 2, 4], (x .&. i) /= 0]
    swap (x, y) = (y, x)

toWgc = unlines . map (\(a, b)-> wgc a ++ "----" ++ wgc b) where
    wgc x = show $ foldl (\s (i, name)-> if (x .&. i) /= 0 then name:s else s) 
                         [] [(1, "wolf"), (2, "goat"), (4, "cabbage"), (8, "farmer")]

prt = putStrLn $ toWgc $ solve
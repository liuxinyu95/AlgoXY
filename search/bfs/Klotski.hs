module Klotski where

import qualified Data.Map as M

-- Heng Dao Li Ma Layout
--  1 A A 2
--  1 A A 2
--  3 4 4 5
--  3 7 8 5
--  6 0 0 9

start = [(1, [(1, 1), (2, 1)]),
         (2, [(1, 4), (2, 4)]),
         (3, [(3, 1), (4, 1)]),
         (4, [(3, 2), (3, 3)]),
         (5, [(3, 4), (4, 4)]),
         (6, [(5, 1)]), (7, [(4, 2)]), (8, [(4, 3)]), (9, [(5, 4)]),
         (10, [(1, 2), (1, 3), (2, 2), (2, 3)])]

solve :: [[M.Map Integer [(Integer, Integer)]]] -> [M.Map Integer [(Integer, Integer)]]
solve [] = [] -- no solution
solve (c@(x:_):cs) | M.lookup 10 x == Just [(4, 2), (4, 3), (5, 2), (5, 3)] = reverse c
                   | otherwise = solve (cs ++ map (:c) xs)
  where
    xs = filter (`notElem` c) [move i dx dy | i <-[1..10],
                                              (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)],
                                              valid i dx dy]
    valid i dx dy = all (\ (x0, y0) -> (M.keys $ M.filter (elem (x0 + dx, y0 + dy)) x) `elem` [[i], []])
                    (maybe [] id $ M.lookup i x)
    move i dx dy = M.update (Just . map (\(x0, y0) -> (x0 + dx, y0 + dy))) i x

--showLayout x

klotski layout = solve [[M.fromList layout]]

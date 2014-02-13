module Klotski where

import qualified Data.Map as M
import Data.Ix
import Debug.Trace (trace)

-- Heng Dao Li Ma Layout
--  1 A A 2
--  1 A A 2
--  3 4 4 5
--  3 7 8 5
--  6 0 0 9

type Layout = M.Map Integer [(Integer, Integer)]
type Move = (Integer, (Integer, Integer))

data Ops = Op Layout [Move] deriving (Eq, Show)

start = [(1, [(1, 1), (2, 1)]),
         (2, [(1, 4), (2, 4)]),
         (3, [(3, 1), (4, 1)]),
         (4, [(3, 2), (3, 3)]),
         (5, [(3, 4), (4, 4)]),
         (6, [(5, 1)]), (7, [(4, 2)]), (8, [(4, 3)]), (9, [(5, 4)]),
         (10, [(1, 2), (1, 3), (2, 2), (2, 3)])]

solve :: [Ops] -> [Layout]-> [Move]
solve [] _ = [] -- no solution
solve (Op x seq : cs) visit | M.lookup 10 x == Just [(4, 2), (4, 3), (5, 2), (5, 3)] = reverse seq
                            | otherwise = solve (trace ((show $ length q) ++"   "++ (show $ length seq)) q) visit'
  where
    ops = expand x visit
    visit' = map (move x) ops ++ visit
    q = cs ++ [Op (move x op) (op:seq) | op <- ops ]

expand :: Layout -> [Layout] -> [Move]
expand x visit = [(i, d) | i <-[1..10], d <- [(0, -1), (0, 1), (-1, 0), (1, 0)],
                           valid i d, move x (i, d) `notElem` visit] where
  valid i d = all (\ p -> let p' = shift p d in
                    inRange ((1, 1), (5, 4)) p' &&
                    (M.keys $ M.filter (elem p') x) `elem` [[i], []])
              (maybe [] id $ M.lookup i x)

move x (i, d) = M.update (Just . map (flip shift d)) i x

shift (y, x) (dy, dx) = (y + dy, x + dx)

--showLayout x

klotski layout = solve [Op (M.fromList layout) []] []

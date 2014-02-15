-- Klotski.hs
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

module Klotski where

import qualified Data.Map as M
import Data.Ix
import Data.List (sort)
import Data.Array
import Numeric (showHex)  -- for pretty output purpose only

-- `Heng Dao Li Ma' Layout
--  1 A A 2
--  1 A A 2
--  3 4 4 5
--  3 7 8 5
--  6 0 0 9

type Point = (Integer, Integer)
type Layout = M.Map Integer [Point]
type Move = (Integer, Point)

data Ops = Op Layout [Move]

board = listArray ((1,1), (5,4)) (replicate 20 0)

start = [(1, [(1, 1), (2, 1)]),
         (2, [(1, 4), (2, 4)]),
         (3, [(3, 1), (4, 1)]),
         (4, [(3, 2), (3, 3)]),
         (5, [(3, 4), (4, 4)]),
         (6, [(5, 1)]), (7, [(4, 2)]), (8, [(4, 3)]), (9, [(5, 4)]),
         (10, [(1, 2), (1, 3), (2, 2), (2, 3)])]

-- Normalize the layout. It removes the piece information and treats them equally.
-- This removes a lot of potential duplicated layout.
layout = sort . map sort . M.elems

-- Avoid attempting mirror layout. It halves the search space.
mirror = M.map (map (\ (y, x) -> (y, 5 - x)))

solve :: [Ops] -> [[[Point]]]-> [Move]
solve [] _ = [] -- no solution
solve (Op x seq : cs) visit | M.lookup 10 x == Just [(4, 2), (4, 3), (5, 2), (5, 3)] = reverse seq
                            | otherwise = solve q visit'
  where
    ops = expand x visit
    visit' = map (layout . move x) ops ++ visit
    q = cs ++ [Op (move x op) (op:seq) | op <- ops ]

expand :: Layout -> [[[Point]]] -> [Move]
expand x visit = [(i, d) | i <-[1..10], d <- [(0, -1), (0, 1), (-1, 0), (1, 0)],
                           valid i d, unique i d] where
  valid i d = all (\p -> let p' = shift p d in
                    inRange (bounds board) p' &&
                    (M.keys $ M.filter (elem p') x) `elem` [[i], []])
              (maybe [] id $ M.lookup i x)
  unique i d = let mv = move x (i, d) in all (`notElem` visit) (map layout [mv, mirror mv])

move x (i, d) = M.update (Just . map (flip shift d)) i x

shift (y, x) (dy, dx) = (y + dy, x + dx)

-- this builds a sequence of moves, e.g. [(1, (0, 1), (3, (-1, 0)), ...]
-- Which means, move 1st piece to right 1 step, then move the 3rd piece up 1 step, ...
klotski = let x = M.fromList start in solve [Op x []] [layout x]

-- the followings are for pretty printing only.

-- !!! Note that this program is a bit SLOW, it takes minutes typically, please wait a
-- while for the result being printed.

toTable = toTab . map (flip showHex "") . elems . toArray where
  toTab [] = []
  toTab xs = let (r, xs') = splitAt 4 xs in r : toTab xs'
  toArray x = board // (M.foldrWithKey (\k ps assoc ->
                                       (map (\p -> (p, k)) ps) ++ assoc) [] x)

output = do
  mapM_ print $ scanl move (M.fromList start) seq
  putStrLn $ "total " ++ (show $ length seq) ++ " steps"
    where
      seq = klotski
      print = putStrLn . unlines . map show . toTable

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

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Queue
import Data.Sequence (Seq((:<|)), (><))
import Data.Array
import Numeric (showHex)  -- for pretty output

-- `Heng Dao Li Ma' Layout
--  1 A A 2
--  1 A A 2
--  3 4 4 5
--  3 7 8 5
--  6 0 0 9

-- piece: [1, 2, ..., 9, A = 10]; 0: free cell

cellOf (y, x) = y * 4 + x

posOf c = (c `div` 4, c `mod` 4)

cellSet = Set.fromList . (map cellOf)

-- move left, right, up, down (by 1)
--     (dy, dx): [(0, -1), (0, 1), (-1, 0), (1, 0)]
--     dc:       [-1, 1, -4, 4]

type Layout = Map.Map Integer (Set.Set Integer) -- {piece : {cells}}
type NormLayout = Set.Set (Set.Set Integer)  -- normalized layout, {{c1, c2}, {c3, c4}, ...}
type Move = (Integer, Integer) -- (piece, dc)

start :: Layout
start = Map.map cellSet $ Map.fromList
        [(1, [(0, 0), (1, 0)]),
         (2, [(0, 3), (1, 3)]),
         (3, [(2, 0), (3, 0)]),
         (4, [(2, 1), (2, 2)]),
         (5, [(2, 3), (3, 3)]),
         (6, [(4, 0)]), (7, [(3, 1)]), (8, [(3, 2)]), (9, [(4, 3)]),
         (10, [(0, 1), (0, 2), (1, 1), (1, 2)])]

end = cellSet [(3, 1), (3, 2), (4, 1), (4, 2)]

-- The normalized the layout removes the piece information (treats pieces equally).
-- normalize :: Layout -> NormLayout
normalize = Set.fromList . Map.elems

-- mirrored layout
-- mirror :: Layout -> Layout
mirror = Map.map (Set.map f) where
  f c = let (y, x) = posOf c in cellOf (y, 3 - x)

-- Build a sequence of moves, e.g. [(1, 1), (3, -4), ...]
-- Which means, move 1st piece to right 1 step, then move the 3rd piece up 1 step, ...
klotski = solve q visited where
  q = Queue.singleton (start, [])
  visited = Set.singleton (normalize start)

-- solve :: (Queue.Seq (Layout, [Move])) -> (Set.Set NormLayout) -> [Move]
solve Queue.Empty _ = []
solve ((x, ms) :<| cs) visited | Map.lookup 10 x == Just end = reverse ms
                               | otherwise = solve q visited'
  where
    q = cs >< (Queue.fromList [(move x op, op:ms) | op <- ops ])
    visited' = foldr Set.insert visited (map (normalize . move x) ops)
    ops = expand x visited

-- expand :: Layout -> (Set.Set NormLayout) -> [Move]
expand x visited = [(i, d) | i <-[1..10], d <- [-1, 1, -4, 4], valid i d, unique i d]
  where
    valid i d = let p = trans d (maybe Set.empty id $ Map.lookup i x) in
                  (not $ any (outside d) p) &&
                  (Map.keysSet $ Map.filter (overlapped p) x) `Set.isSubsetOf` Set.singleton i
    outside d c = c < 0 || c >= 20 || (d == 1 && c `mod` 4 == 0) || (d == -1 && c `mod` 4 == 3)
    unique i d = let ly = move x (i, d) in all (`Set.notMember` visited) [normalize ly, normalize (mirror ly)]

-- move piece i by d in layout x
-- move :: Layout -> (Integer, Integer) -> Layout
move x (i, d) = Map.update (Just . trans d) i x

-- translate a piece by d
-- trans :: Integer -> (Set.Set Integer) -> (Set.Set Integer)
trans d = Set.map (d+)

overlapped :: (Set.Set Integer) -> (Set.Set Integer) -> Bool
overlapped a b = (not . Set.null) $ Set.intersection a b

-- pretty print a layout
board = listArray ((0,0), (4,3)) (replicate 20 0)

toTable :: Layout -> [[String]]
toTable = toTab . map (flip showHex "") . elems . toArray where
  toTab [] = []
  toTab xs = let (r, xs') = splitAt 4 xs in r : toTab xs'
  toArray x = board // (Map.foldrWithKey (\k cs assoc ->
                (map (\c -> (posOf c, k)) (Set.toList cs)) ++ assoc) [] x)

-- !!! Note that this program is a bit SLOW, it takes minutes typically, please wait a
-- while for the result being printed.
output = do
  mapM_ print $ scanl move start ms
  putStrLn $ "total " ++ (show $ length ms) ++ " steps"
    where
      ms = klotski
      print = putStrLn . unlines . map show . toTable

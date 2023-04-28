-- Queens.hs
-- Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
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

module Queens where

import Data.List ((\\), sortOn)
import Data.Set (Set, empty, insert, notMember, size)
import Data.Tuple (swap)

-- DFS find 92 distinct solutions
solve = dfsSolve [[]] [] where
    dfsSolve [] s = s
    dfsSolve (c:cs) s
             | length c == 8 = dfsSolve cs (c:s)
             | otherwise = dfsSolve ([(x:c) | x <- [1..8] \\ c, not $ attack x c] ++ cs) s

    attack x cs = let y = 1 + length cs in
                 any (\(c, r) -> abs(x - c) == abs(y - r)) $ zip (reverse cs) [1..]

-- Leverage Dihedral Group D4 to transform a solution.
-- D4 has 8 symmetries: square (A, B, C, D)
-- id: e,
-- 3 rotations: 90, 180, 270
-- 2 reflection: x, y
-- 2 reflection: AC, BD
uniqueSolve = dfs [[]] (empty :: Set [Int]) where
    dfs [] s = s
    dfs (c:cs) s
      | length c == 8 = dfs cs (uniqueAdd c s)
      | otherwise = dfs ([(x:c) | x <- [1..8] \\ c, not $ attack x c] ++ cs) s
    uniqueAdd c s = if all (`notMember` s) [f c | f <- [id,
        reverse, map (9 - ), -- reflect vertical/horizontal
        trans swap, trans (\(i, j) -> (9 - j, 9 - i)), -- reflect AC, BD
        trans (\(i, j) -> (9 - j, i)),
        trans (\(i, j) -> (9 - i, 9 - j)),
        trans (\(i, j) -> (j, 9 - i))]] then insert c s else s
    trans f xs = snd $ unzip $ sortOn fst $ map f $ zip [1..8] xs
    attack x cs = let y = 1 + length cs in
        any (\(c, r) -> abs(x - c) == abs(y - r)) $ zip (reverse cs) [1..]


-- n queens, extended from 8 to n
queens n = dfsSolve [[]] [] where
    dfsSolve [] s = s
    dfsSolve (c:cs) s
             | length c == n = dfsSolve cs (c:s)
             | otherwise = dfsSolve ([(x:c) | x <- [1..n] \\ c, not $ attack x c] ++ cs) s
    attack x cs = let y = 1 + length cs in
                 any (\(c, r) -> abs(x - c) == abs(y - r)) $ zip (reverse cs) [1..]

-- Maze.hs
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

module Maze where

import Data.Array
import Debug.Trace (trace)

-- This is rather like exhaustive search than DFS
solveMaze m from to = map reverse $ solve from [[]] where
    solve p paths | p == to = map (p:) paths
                  | otherwise = concat [solve p' (map (p:) paths) | 
                                        p' <- adjacent p, not $ visited p' paths]
    adjacent (x, y) = [(x', y') | (x', y') <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], 
                                  inRange (bounds m) (x', y'), m ! (x', y') == 0]
    visited p paths = any (p `elem`) paths

-- This just DFS VISIT the maze, but not work for a solution.
dfsVisit m from to = reverse $ solve [from] [] where
    solve [] path = path
    solve (c:cs) path 
        | c == to = c:path
        | c `elem` path = solve cs path
        | otherwise = solve ((adjacent c) ++ cs) (c:path)
    adjacent (x, y) = [(x', y') | (x', y') <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], 
                                  inRange (bounds m) (x', y'), m ! (x', y') == 0]

-- DFS solve, return ONLY the first solution
dfsSolve m from to = reverse $ solve [[from]] where
    solve [] = []
    solve (c@(p:path):cs)
        | p == to = c -- stop at the first solution
        | otherwise = let os = filter (`notElem` path) (adjacent p) in
                          if os == [] 
                          then solve cs
                          else solve ((map (:c) os) ++ cs) -- try new candidates
    adjacent (x, y) = [(x', y') | (x', y') <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], 
                                  inRange (bounds m) (x', y'), m ! (x', y') == 0]

-- DFS solve, return ALL the solutions
dfsSolveAll m from to = map reverse $ solve [[from]] [] where
    solve [] ss = ss
    solve (c@(p:path):cs) ss
        | p == to = solve cs (c:ss) -- find one solution, go on search
        | otherwise = let os = filter (`notElem` path) (adjacent p) in
                          if os == [] 
                          then solve cs ss
                          else solve ((map (:c) os) ++ cs) ss
    adjacent (x, y) = [(x', y') | (x', y') <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], 
                                  inRange (bounds m) (x', y'), m ! (x', y') == 0]


mz = [[0, 0, 1, 0, 1, 1],
      [1, 0, 1, 0, 1, 1],
      [1, 0, 0, 0, 0, 0],
      [1, 1, 0, 1, 1, 1],
      [0, 0, 0, 0, 0, 0],
      [0, 0, 0, 1, 1, 0]]

mz1 = [[0, 0, 0, 0, 0, 1],
       [1, 0, 1, 1, 0, 1],
       [1, 0, 1, 1, 0, 1],
       [1, 0, 1, 1, 0, 1],
       [1, 0, 0, 0, 0, 0],
       [1, 1, 1, 1, 1, 0]]

maze  = listArray ((1,1), (6, 6)) . concat

test1 = [solveMaze (maze m) (1,1) (6,6) | m <- [mz, mz1]]

test2 = [dfsVisit (maze m) (1,1) (6,6) | m <- [mz, mz1]]

test3 = [dfsSolve (maze m) (1,1) (6,6) | m <- [mz, mz1]]

test4 = [dfsSolveAll (maze m) (1,1) (6,6) | m <- [mz, mz1]]

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
dfsSolve m from to = reverse $ solve [[from]] [] where
    solve [] visit = []
    solve (c@(p:path):cs) visit
        | p == to = c -- stop at the first solution
        | otherwise = let os = filter (`notElem` visit) (adjacent p) in
                      if os == [] 
                      then solve cs visit
                      else solve ((map (:c) os) ++ cs) (p:visit) -- try new candidates
    adjacent (x, y) = [(x', y') | (x', y') <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], 
                                  inRange (bounds m) (x', y'), m ! (x', y') == 0]

-- TODO: DFS returns ALL solution

mz = [[0, 0, 1, 0, 1, 1],
      [1, 0, 1, 0, 1, 1],
      [1, 0, 0, 0, 0, 0],
      [1, 1, 0, 1, 1, 1],
      [0, 0, 0, 0, 0, 0],
      [0, 0, 0, 1, 1, 0]]

maze = listArray ((1,1), (6, 6)) (concat mz)

test = solveMaze maze (1,1) (6,6)

test2 = dfsSolve maze (1,1) (6,6)

test3 = dfsSolve maze (1,1) (6,6)

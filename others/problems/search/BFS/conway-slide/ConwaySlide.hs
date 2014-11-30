module ConwaySlide where

-- BFS solution to Conway slide puzzle
-- A map is introduced to
--    1, record the visited states to avoid duplicated attempts;
--    2, For every state, record its parent state. The initial state has itself as the parent.
-- map, key: state, value: parent state.

-- The state of the sliding puzzle is a list of number. Let the initial state
--
-- 1 ---- [ ] -----7
-- |       |       |
-- 2       |       6
-- |       |       |
-- 3 ----- 4 ------5
--
-- be [0, 1, 2, 3, 4, 5, 6, 7]
-- Where 0 represents the free cell. The goal is to transform to final state
-- [0, 7, 6, 5, 4, 3, 2, 1].
--
-- The rules is to slid any number to free cell as defined as left, right, up, down
-- as below.
--
-- When arrive at the final state, the program backtracks along with the parent state
-- till the initial one with the help of the map. The result is a list of states from
-- the start to the end.

import Data.Map (insert, singleton, notMember, (!))

-- BFS search
-- Input a queue of pairs (state, parent state), and the visited state history

solve [] _ = [] -- no slution
solve (q@(n, _):qs) h | n == [0, 7, 6, 5, 4, 3, 2, 1] = backtrack q h []
                      | otherwise = let (cs, h') = (slide n h) in solve (qs ++ cs) h'

-- Slide possible tiles to the free cell
--   input the current state n, and the visited state history h.
--   Try all movements and filter the duplicated ones by looking up the history map.
--   output a pair, the new candates in (state, parent) form, and the new state history.
slide n h = ([(n', n) | n' <- ns], foldr (\n' h' -> insert n' n h') h ns) where
  ns = [n' | n' <- map ($ n) [left, right, up, down], n' /= [], n' `notMember` h]

-- xxxa0xxx -> xxx0axxx
right (0:xs) = (last xs):(init xs) ++ [0]
right xs = let (as, (_:bs)) = break (==0) xs in (init as) ++ (0 : last as : bs)

-- xxx0axxx -> xxxa0xxx
left xs = reverse $ right $ reverse xs

-- 0xxxaxxx -> axxx0xxx
down (0:a:b:c:d:xs) = d:a:b:c:0:xs
down _ = []

-- axxx0xxx -> 0xxxaxxx
up (a:b:c:d:0:xs) = 0:b:c:d:a:xs
up _ = []

backtrack (n, n') h ns | n == n' = n:ns
                       | otherwise = backtrack (n', h ! n') h (n:ns)

ans = solve [([0..7], [0..7])] (singleton [0..7] [0..7])

-- The answer (35 steps) is as below:
-- ans
--[[0,1,2,3,4,5,6,7],
-- [1,0,2,3,4,5,6,7],[1,2,0,3,4,5,6,7],[1,2,3,0,4,5,6,7],[1,2,3,4,0,5,6,7],
-- [0,2,3,4,1,5,6,7],[7,2,3,4,1,5,6,0],[7,2,3,4,1,5,0,6],[7,2,3,4,1,0,5,6],
-- [7,2,3,4,0,1,5,6],[0,2,3,4,7,1,5,6],[2,0,3,4,7,1,5,6],[2,3,0,4,7,1,5,6],
-- [2,3,4,0,7,1,5,6],[2,3,4,7,0,1,5,6],[0,3,4,7,2,1,5,6],[6,3,4,7,2,1,5,0],
-- [6,3,4,7,2,1,0,5],[6,3,4,7,2,0,1,5],[6,3,4,7,0,2,1,5],[0,3,4,7,6,2,1,5],
-- [3,0,4,7,6,2,1,5],[3,4,0,7,6,2,1,5],[3,4,7,0,6,2,1,5],[3,4,7,6,0,2,1,5],
-- [0,4,7,6,3,2,1,5],[5,4,7,6,3,2,1,0],[5,4,7,6,3,2,0,1],[5,4,7,6,3,0,2,1],
-- [5,4,7,6,0,3,2,1],[0,4,7,6,5,3,2,1],[4,0,7,6,5,3,2,1],[4,7,0,6,5,3,2,1],
-- [4,7,6,0,5,3,2,1],[4,7,6,5,0,3,2,1],[0,7,6,5,4,3,2,1]]

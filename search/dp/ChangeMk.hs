-- ChangeMk.hs
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

module ChangeMk where

import Data.List (group, minimumBy)
import Data.Function (on)
import Data.Sequence (Seq, singleton, index, (|>))

-- [1] ``Dynamic Programming Solution to the Coin Changing Problem''.
--   2004, open course. CSG 713 Advanced algorithms
-- http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf


-- Method 1, Top-down recursive solution without store the sub optimal solutions

solve cs = assoc . change cs

change _ 0 = []
change cs x = minimumBy (compare `on` length) [c:change cs (x - c) | c <- cs, c <= x]

assoc = (map (\cs -> (head cs, length cs))) . group

-- Method 2, Bottom-up dynamic programming solution with finger tree.[1]

changemk x cs = makeChange x $ foldl change' (singleton (0, 0)) [1..x] where
  change' tab i = let sel c = min (1 + fst (index tab (i - c)), c)
                  in tab |> (foldr sel ((x + 1), 0) $ filter (<=i) cs)
  makeChange 0 _ = []
  makeChange x tab = let c = snd $ index tab x in c : makeChange (x - c) tab


coinsUSA = reverse [1, 5, 25, 50, 100]
coinsTest = [1, 2, 4]

-- example:
-- solve coinsUSA 142 -- Don't do this it's takes too long time
-- solve [1, 2, 4] 6 ==> [(2,1),(4,1)]
testChange1 = changemk 142 [1, 5, 25, 50, 100]
testChange2 = changemk 6 [1, 2, 4]

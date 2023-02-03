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

import qualified Data.Set as Set
import Data.Sequence ((|>), singleton, index)

-- [1] ``Dynamic Programming Solution to the Coin Changing Problem''.
--   2004, open course. CSG 713 Advanced algorithms
-- http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf

-- Bottom-up dynamic programming solution with finger tree.[1]

changemk x cs = makeChange x $ foldl fill (singleton (0, 0)) [1..x] where
  fill tab i = tab |> (n, c) where
    (n, c) = minimum $ Set.map lookup $ Set.filter (<= i) cs
    lookup c  = (1 + fst (tab `index` (i - c)), c)
  makeChange 0 _ = []
  makeChange x tab = let c = snd $ tab `index` x in c : makeChange (x - c) tab

cs1 = (Set.fromList [1, 5, 25, 50, 100])
cs2 = (Set.fromList [1, 2, 4])

-- example:
testChange1 = changemk 142 cs1
testChange2 = changemk 6 cs2

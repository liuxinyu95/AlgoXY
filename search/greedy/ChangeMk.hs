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
import Data.List (group)

-- Greedy method to solve the change making problem.

solve x = assoc . change x where
  change 0 _ = []
  change x cs = let c = Set.findMax $ Set.filter (<= x) cs in c : change (x - c) cs
  assoc = (map (\cs -> (head cs, length cs))) . group

example = solve 142 $ Set.fromList [1, 5, 25, 50, 100]

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

-- Top-down recursive solution

solve cs = assoc . change cs

change _ 0 = []
change cs x = minimumBy (compare `on` length) [c:change cs (x - c) | c <- cs, c <= x]

assoc = (map (\cs -> (head cs, length cs))) . group

coinsUSA = reverse [1, 5, 25, 50, 100]
coinsTest = [1, 2, 4]

-- example:
-- solve coinsUSA 142 -- Don't do this it's takes too long time
-- solve [1, 2, 4] 6 ==> [(2,1),(4,1)]

-- SubSetSum.hs
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

module SubSetSum where

import Data.Function (on)
import Data.List

--solve :: (Ord a, Num a)=>[a] -> a -> [[a]]
solve as s | s == 0 = [[]]
           | otherwise = concat [ map (x:) (solve as (s-x)) |  
                                  x <- as, s-x >=0 ]


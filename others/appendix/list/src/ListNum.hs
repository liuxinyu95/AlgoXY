-- ListNum.hs
-- Copyright (C) 2012 Liu Xinyu (liuxinyu95@gmail.com)
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

-- Numeric Representation by List

module ListNum where

import Test.QuickCheck

dec = foldr (\c d -> d * 10 + c) 0

toList n | n < 10 = [n]
         | otherwise = (n `mod` 10) : toList (n `div` 10)

add [] bs = bs
add as [] = as
add (a:as) (b:bs) = if c > 0 then d : add as (add bs [c])
                    else d : add as bs
  where d = (a + b) `mod` 10
        c = (a + b) `div` 10

minus as [] = as
minus (a:as) (b:bs) | a < b = (10 + a - b) : minus (minus as [1]) bs
                    | otherwise = (a - b) : minus as bs

-- test
prop_add :: Int -> Int -> Bool
prop_add x y = (a + b) == (dec $ add (toList a) (toList b)) where
  (a, b) = (abs x, abs y)

prop_minus :: Int -> Int -> Bool
prop_minus x y = (a - b) == (dec $ minus (toList a) (toList b)) where
  a = max (abs x) (abs y)
  b = min (abs x) (abs y)

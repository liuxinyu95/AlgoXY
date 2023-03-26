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
add as [0] = as
add (a:as) (b:bs) = ((a + b) `mod` 10) : add as (add bs [(a + b) `div` 10])

minus as [] = as
minus (a:as) (b:bs) | a < b = (10 + a - b) : minus (minus as [1]) bs
                    | otherwise = (a - b) : minus as bs

mul as = foldr (\b cs -> add (mul1 b as) (0:cs)) [] where
  mul1 0 _ = []
  mul1 1 as = as
  mul1 b [] = []
  mul1 b (a:as) = (b * a `mod` 10) : add [b * a `div` 10] (mul1 b as)

-- test

-- both f (a, b) = (f a, f b)
both = fmap

minMax (a, b) = if a < b then (a, b) else (b, a)

prop_add :: Int -> Int -> Bool
prop_add x y = a + b == (dec $ add (toList a) (toList b)) where
  (a, b) = both abs (x, y)

prop_minus :: Int -> Int -> Bool
prop_minus x y = a - b == (dec $ minus (toList a) (toList b)) where
  (b, a) = minMax $ both abs (x, y)

prop_mul :: Int -> Int -> Bool
prop_mul x y = a * b == (dec $ mul (toList a) (toList b)) where
  (a, b) = both abs (x, y)

testAll = mapM quickCheck [prop_add, prop_minus, prop_mul]

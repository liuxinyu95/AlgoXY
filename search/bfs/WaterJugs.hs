-- WaterJugs.hs
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

module WaterJugs where

import Debug.Trace

-- General solution to 2 jugs:

-- Problem:
-- Given 2 jugs of volumn a and b (a < b), bring up g volumns of water from the river.

-- Constraint:
-- g must be value of n * gcd(a, b), written as gcd(a, b) | g
-- otherwise, it's unsolvable.
-- a and b are not neccessarily relatively prime, for gcd(a, b) = 1, it's possible to
-- bring up any quantity of g.

-- Memo of extended Euclid algorithm:
-- The extended Euclide algorithm does not only give GCD of a and b, but also give one of
-- the linear combination so that

--     a x + b y = gcd(a, b)

-- Define this function as:

--     (d, x, y) = ext-gcd(a, b)

-- Without loss of generality, assume a < b. there exists quotation q and remainder r that

--    b = a q + r

-- Since d = gcd(a, b), it can divide both a and b, thus d can divide r as well.
-- Because r is less than a, we can scale down the problem to find the GCD of a and r:

--    (d, x', y') = ext-gcd(r, a)

-- Where d = x' r + y' a according to the definition of extended Euclid algorithm.

-- Transform b = a q + r to r = b - a q, substitude r in above equation yields:

--    d = x' (b - a q) + y' a   this can be further transform to:
--    d = (y' - x' q) a + x' b

-- This is linear combination of a and b, so that we have:

--    x = y' - x' (b / a)
--    y = x'

-- Beside this recursive case, the edge case happens when a = 0:
--  gcd(a, b) = b = 0 a + 1 b

-- Summarize the above gives the extended Euclide algorithm:
extGcd 0 b = (b, 0, 1)
extGcd a b = let (d, x', y') = extGcd (b `mod` a) a in (d, y' - x' * (b `div` a), x')

-- Method 1, Extended Euclid algorithm.

-- let g = a x + b y.
-- If a < 0, repeatedly increase x by b and decrese y by a till a is greater than 0.
-- Repeat x times:
--    Fill the jug of volumn a;
--    Pour all the water in jug a into jug b. Whenever jug b becomes full, empty it out.
--solve :: Integer -> Integer -> Integer -> [(Integer, Integer)]
solve a b g | g `mod` d /= 0 = [] -- no solution
            | otherwise = solve' (x * g `div` d)
    where
      (d, x, y) = extGcd a b
      solve' x | x < 0 = solve' (x + b)
               | otherwise = pour x [(0, 0)]
      pour 0 ps = reverse ((0, g):ps)
      pour x ps@((a', b'):_) | a' == 0 = pour (x - 1) ((a, b'):ps) -- fill a
                             | b' == b = pour x ((a', 0):ps) -- empty b
                             | otherwise = pour x ((max 0 (a' + b' - b), min (a' + b') b):ps)

-- Method 2, BFS (brute-force)
solve' a b g = bfs [[(0, 0)]] where
  bfs [] = []
  bfs (c:cs) | fst (head c) == g || snd (head c) == g = reverse c
             | otherwise = bfs (cs ++ map (:c) (expand c))
  expand ((x, y):ps) = filter (`notElem` ps) $ map (\f -> f x y)
                           [fillA, fillB, pourA, pourB, emptyA, emptyB]
  fillA _ y = (a, y)
  fillB x _ = (x, b)
  emptyA _ y = (0, y)
  emptyB x _ = (x, 0)
  pourA x y = (max 0 (x + y - b), min (x + y) b)
  pourB x y = (min (x + y) a, max 0 (x + y - a))

-- Some test data:
-- a = 3, b = 5, g = 4
-- a = 4, b = 9, g = 6

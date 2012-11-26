-- Pow.hs
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

-- Compute b^n in O(lg n) time.
-- Refer to SICP, problem 1.16.

module Pow where

-- pure recursive ver.
pow b n  | n == 0 = 1
         | even n = pow (b*b) (n `div` 2)
         | otherwise = b * pow b (n-1)

-- tail-call (tail-recursive) ver, using accumulator
pow' b n = pow1 b n 1 where
  pow1 b n acc | n == 0 = acc
               | even n = pow1 (b*b) (n `div` 2) acc
               | otherwise = pow1 b (n-1) (acc*b)

-- improved tail-call ver.
-- for pow(b, n), denote n in binary format
--   n = a[m]*2^m + a[m-1]*2^(m-1) + ... + a[1]*2 + a[0]
--   a[i] = 1 or 0
--   if a[i] =0, the new base is b*b
--   e.g. b^11 = b^(2^3 + 2 + 1) = b^(2^3)*b^2*b
pow'' b n = pow2 b n 1 where
  pow2 b n acc | n == 0 = acc
               | even n = pow2 (b*b) (n `div` 2) acc
               | otherwise = pow2 (b*b) (n `div` 2) (acc*b)

-- test
test f = map (f 2) [0..10]

-- PerfectN.hs
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

module PerfectN where

-- Sieve of Eratosthenes
-- primes [2..n], generate primes not greater than n. This is a bit faster due to
-- avoid using remainder operation.
primes [] = []
primes (p:ns) = p : (primes $ sieve (p*p) ns) where
  sieve _ [] = []
  sieve m (n:ns) | m < n = sieve (m + p) (n:ns)
                 | m == n = sieve (m + p) ns
                 | otherwise = n : sieve m ns

-- infinite series version
primes' = sieve [2..] where
  sieve (p:ps) = p : (sieve $ filter ((/= 0) . (`rem` p)) ps)

-- Find the perfect numbers not greater than 2^m
-- Euclid-Euler theorem:
--   All even perfect number can be expressed as m(m-1)/2, where
--   m - 1 is Mersenne prime: m - 1 = 2^p - 1, p is prime.
--
-- In other words:
--   2^(p-1)*(2^p-1) for prime p, and Mersenne prime 2^p - 1.
perfectNumbers m =  [mp * 2 ^ (p - 1) |
                     p <- filter (<= m') (reverse ps),
                     let mp = 2^p - 1, prime mp] where
  m' = m `div` 2
  ps = primes [2 .. 2^m']
  prime = (flip) elem ps

-- example
--perfectNumbers 16

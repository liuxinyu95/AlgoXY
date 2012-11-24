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
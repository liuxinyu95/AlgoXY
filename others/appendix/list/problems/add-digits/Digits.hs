module Digits where

import Test.QuickCheck

add xs ys = add' xs ys 0 where
  add' [] [] c = if c == 0 then [] else [c]
  add' [] ys@(y:ys') c = if c == 0 then ys else ((y + c) `mod` 10) : add' [] ys' ((y+c) `div` 10)
  add' xs [] c = add' [] xs c
  add' (x:xs) (y:ys) c = ((x + y + c) `mod` 10) : add' xs ys ((x + y + c) `div` 10)

fromInt 0 = []
fromInt n = (n `mod` 10):fromInt (n `div` 10)

-- examples
example = and [zs == add xs ys | (xs, ys, zs) <- [([], [], []), ([], [1, 3], [1, 3]), ([3, 1, 5], [5, 9, 2], [8, 0, 8]), ([2], [9, 9, 9], [1, 0, 0, 1])]]

--test
prop_add ::Int -> Int -> Bool
prop_add x y = let z = abs x + abs y in fromInt z == fromInt (abs x) `add` fromInt (abs y)

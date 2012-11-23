-- ACM/ICPC, POJ 1218
-- http://poj.org/problem?id=1218
module DrunkJailer where

import Test.QuickCheck

-- Naive brute force ver.
solve n = sum $ map snd $ foldl proc (zip [1..n] (repeat 0)) [1..n] where
    proc xs i = map (switch i) xs
                
switch i (j, x) = if j `mod` i == 0 then (j, 1 - x) else (j, x)

-- Naive ver, without explicitly using folding, and zip
solve' = sum . (map snd) . proc  where
    proc n = operate [1..n] $ map (\i -> (i, 0)) [1..n]
    operate [] xs = xs
    operate (i:is) xs = operate is (map (switch i) xs)

-- smart ver.
solve'' = floor . sqrt . fromIntegral

-- induction
test = map solve [1..100]
test' = map solve' [1..100]

-- test
prop_solve :: Int -> Bool
prop_solve n = let m = if n `elem` [1..1000] then abs(n) else 1 in solve m == solve'' m
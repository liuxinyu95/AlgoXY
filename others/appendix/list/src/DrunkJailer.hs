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

-- induction for which cells(lights) are on
--   results 100 ==> [1,4,9,16,25,36,49,64,81,100]
results n = map fst $ filter (\x -> 1 == snd x) $ foldl proc (zip [1..n] (repeat 0)) [1..n] where
    proc xs i = map (switch i) xs
                
-- Key fact:
--   The numbers which have odd number of factors is left as 1.
--   Only perfect square numbers have odd number of factors.
--      Proof: for any positive number N, if p is factor of N, there
--      exists integer q that, N = p * q, so we add 2 factors to the set of factors of N
--      if and only if p != q. otherwise, p = q leads to perfect squre number, and  
--      we add 1 to the set of factors, so that
--      the size of factor set is odd.
                
-- test
prop_solve :: Int -> Bool
prop_solve n = let m = if n `elem` [1..1000] then abs(n) else 1 in solve m == solve'' m
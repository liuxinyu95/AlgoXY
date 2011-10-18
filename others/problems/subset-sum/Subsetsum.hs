import Data.List

naiveSolve xs n = filter (\ys->n == sum ys) $ tail $ subsequences xs

solve :: (Num a)=>[a] -> a -> [[a]]
solve [] n = []
solve (x:xs) n = if x == n then [x]:xss else xss where
    xss = solve xs n ++ map (x:) (solve xs (n-x))

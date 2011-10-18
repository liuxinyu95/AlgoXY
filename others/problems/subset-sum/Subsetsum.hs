import Data.List

naiveSolve xs n = filter (\ys->(not $ null ys) && n == sum ys) $ subsequences xs

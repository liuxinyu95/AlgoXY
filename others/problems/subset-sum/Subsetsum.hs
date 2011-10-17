import Data.List

solve xs n = filter (\ys->n == sum ys) $ subsequences xs

testAll = map (solve lst) [-12..13] where 
    lst = [-7, -3, -2, 5, 8]
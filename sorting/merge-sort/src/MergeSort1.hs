module MergeSort1 where

mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort as) (mergesort bs) where
    (as, bs) = split xs

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:xs', y:ys') where (xs', ys') = split xs

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x >  y = y : merge (x:xs) ys

test = mergesort [5, 9, 1, 0, 5, 4, 3, 1, 2, 3]
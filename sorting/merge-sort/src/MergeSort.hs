module MergeSort where

--merge :: (Ord a)=>([a], Int)->([a], Int) -> ([a], Int)
merge ([], n) (ys, m) = (ys, n+m)
merge (xs, n) ([], m) = (xs, n+m)
merge ((x:xs), n) ((y:ys), m) = 
    if x< y then let (xs', n') = merge (xs, n) ((y:ys), m) in (x:xs', n')
    else let (xs', n') = merge ((x:xs), n) (ys, m) in (y:xs', n'+1)

--sort' :: (Ord a)=>[([a], Int)] -> ([a], Int)
sort' [] = ([], 0)
sort' [(xs, n)] = (xs, n)
sort' xss = sort' $ mergePairs xss

--mergePairs :: (Ord a)=>[([a], Int)]->[([a], Int)]
mergePairs [] = []
mergePairs [xs] = [xs]
mergePairs (xs:ys:xss) = merge xs ys : mergePairs xss

--sort :: (Ord a)=>[a]->([a], Int)
sort = sort' . map (\x->([x], 0))

test = sort [5, 9, 1, 0, 5, 4, 3, 1, 2, 3]
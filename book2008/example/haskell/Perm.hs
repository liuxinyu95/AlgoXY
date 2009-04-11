module Permutation where

import Data.List

permutationAll [] = [[]]     
permutationAll xs = [x:ys | x <- xs, ys <- permutationAll (delete x xs)]

permutation xs n r= if length xs <= n-r
	then [[]]
	else [x:ys | x <- xs, ys <- permutation (delete x xs) n r]

perm xs = permutation xs n n where n=length xs

order f s = at s x `f` at s (1+x) where
    x = minimum s
    at (y:ys) v = if v==y then 1 else 1 + at ys v

delete_min xs = filter (\x->not (x==minimum xs)) xs

hasPattern [] _ = True
hasPattern ('A':ps) s = order (<) s && hasPattern ps (delete_min s)
hasPattern ('B':ps) s = order (>) s && hasPattern ps (delete_min s)

enumStr1 pattern = filter f (perm [1..(1+length pattern)]) where
    f = hasPattern pattern

count1 = length.enumStr1

enumStrR from [] = [[from]]

enumStrR from ('A':ps) = foldl (++) [] (map f (enumStrR (from+1) ps)) where
    f = insert_before from
    insert_before y (x:xs) = if x==y+1 
        then [y:x:xs]
        else [y:x:xs]++ (map (x:) (insert_before y xs))

enumStrR from ('B':ps) = foldl (++) [] (map f (enumStrR (from+1) ps)) where
    f = insert_after from
    insert_after y (x:xs) = if x==y+1 
        then map (x:) (insert_any y xs)
        else map (x:) (insert_after y xs) where
            insert_any y [] = [[y]]
            insert_any y (x:xs) = [y:x:xs]++(map (x:) (insert_any y xs))

enumStr2 = enumStrR 1

--get ab
enumStr3 ab = sum $ foldl f [1] ab
    where f last 'A' = scanl (+) 0 last
          f last 'B' = scanr (+) 0 last 


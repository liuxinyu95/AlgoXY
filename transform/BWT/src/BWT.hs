module BWT where

import Data.List

-- Algorithm 1, Naive version

bwt :: (Ord a)=>[a]->([a], Int)
bwt s = (map last m, i) where
    m = sort [ drop i s ++ take i s | i<-[1..length s]]
    (Just i) = elemIndex s m

ibwt :: (Ord a)=> ([a], Int) -> [a]
ibwt (r, i) = m !! i where
    m = iterate f (replicate n []) !! n
    f = sort . zipWith (:) r
    n = length r

-- Algorithm 2, 
module BWT where

import Data.List
import Data.Function (on)
import Data.Array
import Test.QuickCheck

-- Algorithm 1, Naive version

-- O( n^2 \lg n)
bwt :: (Ord a)=>[a]->([a], Int)
bwt s = (map last m, i) where
    m = sort [ drop i s ++ take i s | i<-[1..length s]]
    (Just i) = elemIndex s m

-- O( n^3 \lg n )
ibwt :: (Ord a)=> ([a], Int) -> [a]
ibwt (r, i) = m !! i where
    m = iterate f (replicate n []) !! n
    f = sort . zipWith (:) r
    n = length r

-- Algorithm 2, Based on random access idea,
--   However, it performs bad on list.

-- O( n^2 \lg n) if (!!) takes O(n) time
bwt' :: (Ord a)=> [a] -> ([a], Int)
bwt' s =  (l, i) where
    l = map (\i->s !! ((i-1) `mod` n)) ids
    (Just i) = elemIndex 0 ids
    ids = map snd $ sort $ zip rots [0..]
    rots = init $ zipWith (++) (tails s) (inits s) -- only non-empties
    n = length s

-- O( n^2 ) if (!!) takes O(n) time
ibwt' :: (Ord a) => ([a], Int) -> [a]
ibwt' (r, i) =  fst $ iterate f ([], i) !! n where
    t = map snd $ sort $ zip r [0..]
    f (s, j) = let j' = t !! j in (s++[r !! j'], j')
    n = length r

-- Algorithm 2', Array version.

-- O( n \lg n), siince (!) takes constant O(1) time.
ibwt'' :: (Ord a) => ([a], Int) -> [a]
ibwt'' (r, i) =  fst $ iterate f ([], i) !! n where
    t = listArray (0, n-1) $ map snd $ sort $ zip r [0..]
    ra = listArray (0, n-1) r
    f (s, j) = let j' = t ! j in (s++[ra ! j'], j')
    n = length r


-- Algorithm 3, do we have ideal one?

-- test

prop_ibwt :: [Int] -> Property
prop_ibwt xs = not (null xs) ==> xs == (ibwt $ bwt xs)

prop_ibwt' :: [Int] -> Property
prop_ibwt' xs = not (null xs) ==> xs == (ibwt' $ bwt' xs)

prop_ibwt'' :: [Int] -> Property
prop_ibwt'' xs = not (null xs) ==> xs == (ibwt'' $ bwt' xs)

--prop_rbwt :: [Int] -> Property
--prop_rbwt xs = not (null xs) ==> xs == (rbwt $ bwt xs)


module SelSort where
  
import Test.QuickCheck
import Data.List

-- Tail recursive selection sort
ssort [] = []
ssort xs = x : ssort xs' where
  (x, xs') = extractMin xs
  
-- Tail recursive extract the minimum
extractMin (x:xs) = min' [] x xs where
  min' ys m [] = (m, ys) -- Note: ys is in reversed order!
  min' ys m (x:xs) = if m < x then min' (x:ys) m xs else min' (m:ys) x xs
  
-- Naive selection sort
ssort' [] = []
ssort' xs = x : ssort' xs' where
  (x, xs') = extractMin' xs

extractMin' [x] = (x, [])
extractMin' (x:xs) = if x < m then (x, xs) else (m, x:xs') where
  (m, xs') = extractMin' xs
  
-- Naive cock tail sort
csort :: (Ord a) => [a] -> [a]
csort [] = []
csort [x] = [x]
csort xs = mi : csort xs' ++ [ma] where
  (mi, xs', ma) = extractMinMax xs
  
extractMinMax [x, y] = (min x y, [], max x y)
extractMinMax (x:xs) | x < mi = (x, mi:xs', ma)
                     | ma < x = (mi, ma:xs', x)
                     | otherwise = (mi, x:xs', ma)
  where (mi, xs', ma) = extractMinMax xs
        
-- Variants of cock tail sort, avoid ++[max] which performs poor (linear time)
        
-- Method 1, at any time we have the invariant:
--
--  as ++ [min] ++ xs ++ [max] ++ bs
--  where:
--    as: sorted small ones so far;
--    bs: sorted big ones so far;
--    min: the minimum of the unsorted part;
--    max: the maximum of the unsorted part;
--    xs: the rest of the unsorted part

csort' xs = cocktail [] xs [] where
  cocktail as [] bs = as ++ bs
  cocktail as [x] bs = as ++ [x] ++ bs
  cocktail as xs bs = let (mi, xs', ma) = extractMinMax xs
                      in cocktail (as ++ [mi]) xs' (ma:bs)

csort'' xs = cocktail [] xs [] where
  cocktail as [] bs = reverse as ++ bs
  cocktail as [x] bs = reverse (x:as) ++ bs
  cocktail as xs bs = let (mi, xs', ma) = extractMinMax xs 
                      in cocktail (mi:as) xs' (ma:bs)
  
prop_ssort :: [Int] -> Bool
prop_ssort xs = (sort xs) == (ssort xs)

prop_ssort' :: [Int] -> Bool
prop_ssort' xs = (sort xs) == (ssort' xs)

prop_csort :: [Int] -> Bool
prop_csort xs = (sort xs) == (csort xs)

prop_csort' :: [Int] -> Bool
prop_csort' xs = (sort xs) == (csort' xs)

prop_csort'' :: [Int] -> Bool
prop_csort'' xs = (sort xs) == (csort'' xs)
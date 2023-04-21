module Dequeue where

import Test.QuickCheck

data DeQueue a = DQ [a] Int [a] Int

empty = DQ [] 0 [] 0

isEmpty (DQ _ m _ n) = m == 0 && n == 0

pushL x (DQ f m r n) = DQ (x:f) (m + 1) r n

pushR (DQ f m r n) x = DQ f m (x:r) (n + 1)

popL q@(DQ [] _ [] _) = (Nothing, q)
popL (DQ [] _ r n) = let m = n `div` 2
                         (as, bs) = splitAt m r in
                       popL (DQ (reverse bs) (n - m) as m)
popL (DQ (x:f) m r n) = (Just x, DQ f (m - 1) r n)

popR q@(DQ [] _ [] _) = (q, Nothing)
popR (DQ f m [] _) = let n = m `div` 2
                         (as, bs) = splitAt n f in
                       popR (DQ as n (reverse bs) (m - n))
popR (DQ f m (x:r) n) = (DQ f m r (n - 1), Just x)

toList (DQ f _ r _) = f ++ reverse r

prop_queue :: [Int] -> Bool
prop_queue xs = (toList as) == bs where
  (as, bs) = foldr test (empty, []) xs
  test x (q, lst) | x `mod` 4 == 0 = (pushL x q, x:lst)
                  | x `mod` 4 == 1 = (pushR q x, lst ++ [x])
                  | x `mod` 4 == 2 = if null lst then (q, lst) else (snd (popL q), tail lst)
                  | otherwise = if null lst then (q, lst) else (fst (popR q), init lst)

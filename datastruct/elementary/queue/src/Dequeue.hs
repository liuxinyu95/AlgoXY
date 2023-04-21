module Dequeue where

import Test.QuickCheck

empty = ([], [])

isEmpty ([], []) = True
isEmpty _ = False

pushL x (f, r) = (x:f, r)

pushR (f, r) x = (f, x:r)

popL ([], []) = (Nothing, empty)
popL ([],  r) = let (as, bs) = splitAt (length r `div` 2) r in
                  popL (reverse bs, as)
popL (x:f, r) = (Just x, (f, r))

popR ([], []) = (empty, Nothing)
popR (f , []) = let (as, bs) = splitAt (length f `div` 2) f in
                  popR (as, reverse bs)
popR (f, x:r) = ((f, r), Just x)

toList (f, r) = f ++ reverse r

prop_queue :: [Int] -> Bool
prop_queue xs = (toList as) == bs where
  (as, bs) = foldr test (empty, []) xs
  test x (q, lst) | x `mod` 4 == 0 = (pushL x q, x:lst)
                  | x `mod` 4 == 1 = (pushR q x, lst ++ [x])
                  | x `mod` 4 == 2 = if null lst then (q, lst) else (snd (popL q), tail lst)
                  | otherwise = if null lst then (q, lst) else (fst (popR q), init lst)

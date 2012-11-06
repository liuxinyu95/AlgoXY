module ListEx where

import Data.List
import Test.QuickCheck -- for verification purpose only

atR :: [a] -> Int -> a
atR xs i = get xs (drop i xs) where
  get (x:_) [_] = x
  get (_:xs) (_:ys) = get xs ys
  
insertAt :: [a] -> Int -> a -> [a]
insertAt xs 0 y = y:xs
insertAt [] i y = [y]
insertAt (x:xs) i y = x : insertAt xs (i-1) y

prop_rindex :: [Int] -> Bool
prop_rindex xs = xs == (map (atR xs) $ reverse [0..length xs -1])

prop_insertAt :: [Int] -> Int -> Int -> Property
prop_insertAt xs i x = (0 <= i) ==> (insertAt xs i x) == (let (as, bs) = splitAt i xs in as ++ x:bs)
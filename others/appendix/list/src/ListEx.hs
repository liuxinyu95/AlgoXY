module ListEx where

import Data.List
import Test.QuickCheck -- for verification purpose only

atR :: [a] -> Int -> a
atR xs i = get xs (drop i xs) where
  get (x:_) [_] = x
  get (_:xs) (_:ys) = get xs ys
  
prop_rindex :: [Int] -> Bool
prop_rindex xs = xs == (map (atR xs) $ reverse [0..length xs -1])
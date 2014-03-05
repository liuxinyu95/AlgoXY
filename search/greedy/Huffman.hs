module Huffman where

import Data.Function (on)

-- Huffman's original article.
-- D.A. Huffman, "A Method for the Construction of Minimum-Redundancy Codes", Proceedings of the I.R.E., September 1952, pp 1098¨C1102.

data HTr w a = Empty | Leaf w a | Branch w (HTr w a) (HTr w a)
                       deriving Show

weight (Leaf w _) = w
weight (Branch w _ _) = w

instance Ord w => Ord (HTr w a) where
  compare = compare `on` weight

instance Eq w => Eq (HTr w a) where
  (==) = (==) `on` weight

build [x] = x
build xs = build ((Branch (weight x + weight y) x y) : xs') where
  (x, y, xs') = extract xs

-- extract the 2 elements with the smallest weight.
extract (x:y:xs) = min2 x y xs [] where
  min2 x y [] xs = (x, y, xs)
  min2 x y (z:zs) xs | z < min x y = min2 z (min x y) zs ((max x y) : xs)

testData = [("A", 8), ("B", 3), ("C", 1), ("D", 1), ("E", 1), ("F", 1), ("G", 1), ("H", 1)]

huffman = build . map (\(c, w) -> Leaf w c)

--TODO: Method 1 using heap, so that we can always pop the min one.
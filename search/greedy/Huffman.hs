module Huffman where

import Data.Function (on)
import Data.Map (fromList, (!)) -- to store the code table

-- Huffman's original article.
-- D.A. Huffman, "A Method for the Construction of Minimum-Redundancy Codes", Proceedings of the I.R.E., September 1952, pp 1098¨C1102.

-- Definition of Huffman tree, every character is augmented with a weight
data HTr w a = Leaf w a | Branch w (HTr w a) (HTr w a)
                       deriving Show

weight (Leaf w _) = w
weight (Branch w _ _) = w

-- The Huffman tree node can be compared by weight
instance Ord w => Ord (HTr w a) where
  compare = compare `on` weight

instance Eq w => Eq (HTr w a) where
  (==) = (==) `on` weight

-- Method 1, building the Huffman tree by repeatedly extracting the 2 trees with
-- the smallest weight and merge.
build [x] = x
build xs = build ((Branch (weight x + weight y) x y) : xs') where
  (x, y, xs') = extract xs

-- Extract the 2 elements with the smallest weight.
extract (x:y:xs) = min2 (min x y) (max x y) xs [] where
  min2 x y [] xs = (x, y, xs)
  min2 x y (z:zs) xs | z < x = min2 z x zs (y:xs)
                     | y < z = min2 x y zs (z:xs)
                     | otherwise = min2 x z zs (y:xs)

-- Build Huffman tree from an associcate list of character and weight
huffman = build . map (\(c, w) -> Leaf w c)

-- Build the code table from a Huffman tree by traversing it
code tr = fromList $ traverse [] tr where
  traverse bits (Leaf _ c) = [(c, bits)]
  traverse bits (Branch _ l r) = (traverse (bits ++ [0]) l) ++ (traverse (bits ++ [1]) r)

-- Encode a text with code table dict
encode dict = concatMap (dict !)

-- Method 2, building Huffman tree by using heap, repeatedly pop the 2 trees
-- with the smallest weight and merge.

-- TODO: create a freq function, which count the charactor histogram from a text.

testData = [('A', 8), ('B', 3), ('C', 1), ('D', 1), ('E', 1), ('F', 1), ('G', 1), ('H', 1)]

-- examples:
--   encode (code $ huffman testData) "ABC"
module Huffman where

import Data.Function (on)
import Data.Map (fromList, (!)) -- only for storing the code table
import Data.List (sort, group)

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
huffman :: (Num a, Ord a) => [(b, a)] -> HTr a b
huffman = build . map (\(c, w) -> Leaf w c)

-- Build the code table from a Huffman tree by traversing it
code tr = fromList $ traverse [] tr where
  traverse bits (Leaf _ c) = [(c, bits)]
  traverse bits (Branch _ l r) = (traverse (bits ++ [0]) l) ++ (traverse (bits ++ [1]) r)

-- Encode text with a code table
encode dict = concatMap (dict !)

-- Method 2, building Huffman tree by using heap, repeatedly pop the 2 trees
-- with the smallest weight and merge.

-- Decode with a Huffman tree
decode tr cs = find tr cs where
  find (Leaf _ c) ws = c : find tr ws
  find (Branch _ l r) (w:ws) = if w == 0 then find l ws else find r ws
  find _ [] = [] -- won't handle (Leaf _ c) []

-- Auxliary function,
--   count the occurrent of every character to build the histogram of a text.
freq :: (Ord a, Eq a) => [a] -> [(a, Int)]
freq = map (\x -> (head x, length x)) . group . sort

-- examples:

testData = [('A', 8), ('B', 3), ('C', 1), ('D', 1), ('E', 1), ('F', 1), ('G', 1), ('H', 1)]
testEncode = encode (code $ huffman testData) "ABC"
testTree = huffman $ freq "hello wired world"
testCode = encode (code testTree) "hello"
testDecode = decode testTree testCode


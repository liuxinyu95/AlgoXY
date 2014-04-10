{-
 Huffman.hs
 Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}
module Huffman where

import Data.Function (on)
import Data.Map (fromList, (!)) -- only for storing the code table
import Data.List (sort, group)
import qualified Heap as Heap

-- D.A. Huffman, "A Method for the Construction of Minimum-Redundancy Codes", Proceedings of the I.R.E., September 1952, pp 1098¨C1102.

-- Definition of Huffman tree, every character is augmented with a weight
data HTr w a = Leaf w a | Branch w (HTr w a) (HTr w a)
                       deriving Show

weight (Leaf w _) = w
weight (Branch w _ _) = w

merge x y = Branch (weight x + weight y) x y

-- The Huffman tree node can be compared by weight
instance Ord w => Ord (HTr w a) where
  compare = compare `on` weight

instance Eq w => Eq (HTr w a) where
  (==) = (==) `on` weight

-- Method 1, building the Huffman tree by repeatedly extracting the 2 trees with
-- the smallest weight and merge.
build [x] = x
build xs = build ((merge x y) : xs') where
  (x, y, xs') = extract xs

-- Extract the 2 elements with the smallest weight.
extract (x:y:xs) = min2 (min x y) (max x y) xs [] where
  min2 x y [] xs = (x, y, xs)
  min2 x y (z:zs) xs | z < y = min2 (min z x) (max z x) zs (y:xs)
                     | otherwise = min2 x y zs (z:xs)

-- Build Huffman tree from an associcate list of character and weight
huffman :: (Num a, Ord a) => [(b, a)] -> HTr a b
huffman = build . map (\(c, w) -> Leaf w c)

-- Build the code table from a Huffman tree by traversing it
code tr = fromList $ traverse [] tr where
  traverse bits (Leaf _ c) = [(c, bits)]
  traverse bits (Branch _ l r) = (traverse (bits ++ [0]) l) ++ (traverse (bits ++ [1]) r)

-- Encode text with a code table
encode dict = concatMap (dict !)

-- Method 2, build Huffman tree by using heap.
-- Repeatedly pop 2 trees from the heap to merge.
huffman' :: (Num a, Ord a) => [(b, a)] -> HTr a b
huffman' = build' . Heap.fromList . map (\(c, w) -> Leaf w c) where
  build' h = reduce (Heap.findMin h) (Heap.deleteMin h)
  reduce x Heap.E = x
  reduce x h = build' $ Heap.insert (Heap.deleteMin h) (merge x (Heap.findMin h))

-- Method 3, If the symbol-weight assoc list is ordered, Huffman tree can be
-- built in linear time with a queue.
huffman'' :: (Num a, Ord a) => [(b, a)] -> HTr a b
huffman'' = reduce . wrap . sort . map (\(c, w) -> Leaf w c) where
  wrap xs = delMin ([], xs)
  reduce (x, ([], [])) = x
  reduce (x, h) = let (y, (q, xs)) = delMin h in reduce $ delMin (q ++ [merge x y], xs)
  delMin ([], (x:xs)) = (x, ([], xs))
  delMin ((q:qs), []) = (q, (qs, []))
  delMin ((q:qs), (x:xs)) | q < x = (q, (qs, (x:xs)))
                          | otherwise = (x, ((q:qs), xs))

-- Decode with a Huffman tree
decode tr cs = find tr cs where
  find (Leaf _ c) [] = [c]
  find (Leaf _ c) bs = c : find tr bs
  find (Branch _ l r) (b:bs) = find (if b == 0 then l else r) bs

-- Auxliary function,
--   count the occurrence of every character to build the histogram of a text.
freq :: (Ord a, Eq a) => [a] -> [(a, Int)]
freq = map (\x -> (head x, length x)) . group . sort

-- Pretty print
prt (Leaf w c) = "(. " ++ [c] ++ (show w) ++ " .)"
prt (Branch w l r) = "(" ++ (prt l) ++ " " ++ (show w) ++ " " ++ (prt r) ++ ")"

-- examples:

testData = [('A', 8), ('B', 3), ('C', 1), ('D', 1), ('E', 1), ('F', 1), ('G', 1), ('H', 1)]
testEncode = encode (code $ huffman testData) "ABC"
testTree = huffman $ freq "hello wired world"
testCode = encode (code testTree) "hello"
testDecode = decode testTree testCode
testTree' = huffman' $ freq "hello wired world"
testCode' = encode (code testTree') "hello"
testDecode' = decode testTree' testCode'
testTree'' = huffman'' $ freq "hello wired world"
testCode'' = encode (code testTree'') "hello"
testDecode'' = decode testTree'' testCode''

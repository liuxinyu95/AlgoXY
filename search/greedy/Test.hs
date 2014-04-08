module Test where

import Huffman

toDot (Leaf w c) = "(. " ++ [c] ++ (show w) ++ " .)"
toDot (Branch w l r) = "(" ++ (toDot l) ++ " " ++ (show w) ++ " " ++ (toDot r) ++ ")"


module Huffman where

-- Huffman's original article.
-- D.A. Huffman, "A Method for the Construction of Minimum-Redundancy Codes", Proceedings of the I.R.E., September 1952, pp 1098¨C1102.

data HTr w a = Empty | Node w a (HTr w a) (HTr w a)
                       deriving Eq, Show

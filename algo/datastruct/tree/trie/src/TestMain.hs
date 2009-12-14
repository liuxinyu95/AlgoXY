module Main where

import qualified IntTrie
import qualified IntPatricia
import qualified Trie
import qualified Patricia

main = do
  putStrLn "Test Integer Trie..."
  putStrLn IntTrie.testIntTrie
  putStrLn "\nTest Integer Patricia..."
  putStrLn IntPatricia.testIntTree
  putStrLn "\nTest Trie..."
  putStrLn Trie.testTrie
  putStrLn "\nTest Patricia..."
  putStrLn Patricia.testPatricia

{-
    TestMain.hs, Main test program (entry) for Trie and Patricia Tree.
    Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

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

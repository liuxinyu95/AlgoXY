{-
    Fr2dot.hs, Convert a Forest description string to dot script.
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

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Control.Monad (mapM_)
import Data.List (concatMap)
import System.IO (writeFile)
import Data.Char (isSpace)

-- For each tree, it is described in pre-order.
-- Example description string of a forest of CLRS Figure 19.5(a):
--   (12), (7, (25)), (15, ((28, (41)), 33))
-- 

-- Definition of K-ary node
data Node a = Node a [Node a] deriving (Eq, Show)

-- Definition of Forest
type Forest a = [Node a]

-- parsers

forest = node `sepBy` (char ',')

node = do
  char '('
  elem <- key 
  char ','
  ts<- forest
  char ')'
  return (Node elem ts)

key = many (noneOf ",()")

parseArgs :: [String] -> (String, String)
parseArgs [fname, s] = (fname, s)
parseArgs _ = error "wrong usage\nexample:\nfr2dot output.dot \"(12), (7, (25)), (15, ((28, (41)), 33))\""

-- convert a node to dot script format
toDot (Node x ts) prefix = prefix'++"[label=\""++x++"\"];\n" ++
                            (concatMap (\t->toDot t prefix') ts) ++
                            (defCons ts prefix')
                                where prefix' = prefix ++ x

-- define connections among nodes in dot format
defCons ts prefix = concatMap f ts where
    f (Node x _) = prefix++"->"++prefix++x++";\n"

-- generate dot script from a parsed forest
genDot fname (Right f) = writeFile fname dots >> putStrLn dots
    where
      dots = "digraph G{\n\tnode\n"++(addTab $ toDot f "t")++"}"
      addTab s = unlines $ map ("\t"++) (lines s)

-- tests
testParse = mapM_ (parseTest node) toks where
    toks = map (filter (not.isSpace))
           ["(12), (7, (25)), (15, ((28, (41)), 33))"]

testToDot = putStrLn $ toDot (Node "C" [Node "A" [],Node "B" []]) "t"

main = do
  args <- getArgs
  let (fname, s) = parseArgs args
  genDot fname (parse node "unknown" (filter (not.isSpace) s))

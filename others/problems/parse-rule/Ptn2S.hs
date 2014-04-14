-- Ptn2S.hs
-- Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Ptn2S where

import Text.ParserCombinators.Parsec
import Data.Char (isSpace)

-- Syntax abstraction
data Term = Tk String | Lst [Rule] deriving (Show)
type Rule = [Term]

-- {a # b} c {d # e {f # g}} 

-- parser
rule = many1 term

term = (try tk) <|> lst 

lst = do
  char '{'
  xs <- rule `sepBy` (char '#')
  char '}'
  return (Lst xs)

tk = do
  x <- many1 (noneOf "#{}")
  return (Tk x)

strip = filter (not.isSpace)

testParse s = parse rule "unknow" (strip s)

test1 = testParse "{a # b} c {d # e {f # g}}"

gen :: Rule -> [String]
gen rs = foldl merge [] (map genTerm rs) where
    merge [] ys = ys
    merge xs [] = xs
    merge xs ys = [x++y | x<-xs, y<-ys]

genTerm :: Term -> [String]
genTerm (Tk s) = [s]
genTerm (Lst rs) = foldl (++) [] (map gen rs)

solve s = f $ parse rule "unknown" (strip s) where
    f (Right r) = gen r

test2 = solve "{a # b} c {d # e {f # g}}"

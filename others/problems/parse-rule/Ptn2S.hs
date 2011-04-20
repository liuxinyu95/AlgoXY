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
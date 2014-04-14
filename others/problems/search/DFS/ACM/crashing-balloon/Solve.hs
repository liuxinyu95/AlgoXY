-- Solve.hs
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

-- ACM/ICPC, ZOJ 1003
-- [1]. http://acm.zju.edu.cn/onlinejudge/showProblem.do?problemCode=1003

-- Soluton: 
--  Suppose a > b, 
--  for any valide factor decomposition of b, there exists a valid decomposition of a => a
--  Otherwise => b

valid a b [] = a ==1 || b /= 1
valid a b (x:xs) | b `mod` x == 0 && (not $ valid a (b `div` x) xs) = False
                 | a `mod` x == 0 && valid (a `div` x) b xs = True
                 | otherwise = valid a b xs

judge a b = if valid a b [2..100] then a else b

main = interact judgeInput where
  judgeInput = unlines . map doJudge . lines
  doJudge ln = let (a:b:_) = map (read::(String->Integer)) (words ln) in 
    show $ judge (max a b) (min a b)

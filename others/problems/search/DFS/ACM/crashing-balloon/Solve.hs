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

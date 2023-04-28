module ConwaySlide where

-- Slide from [0..7] to [0, 7..1]
solve = dfs [[start]] where
  dfs [] = []
  dfs (c:cs)
    | head c == end = reverse c
    | otherwise = dfs ((map (:c) $ moves c) ++ cs)

start = [0..7]
end = 0:[7,6..1]

moves (s:visited) = filter (`notElem` visited) [fwd s, bk s, cut s]

fwd [] = []
fwd (0:x:xs) = x:0:xs
fwd (x:xs) = x : fwd xs
bk [] = []
bk (x:0:xs) = 0:x:xs
bk (x:xs) = x : bk xs
cut xs = case splitAt 4 xs of
  ((0:as), (x:bs)) -> (x:as) ++ (0:bs)
  ((x:as), (0:bs)) -> (0:as) ++ (x:bs)
  _ -> xs

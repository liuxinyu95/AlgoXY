module ConwaySlide where

-- Slide from [0..7] to [0, 7..1]
start = [0..7]
end = 0:[7,6..1]

-- This outputs a solution with 12948 steps for Conway slide puzzle.
solve1 = dfs [[start]] where
  dfs [] = []
  dfs (c:cs)
    | head c == end = reverse c
    | otherwise = dfs ((map (:c) $ moves c) ++ cs)

-- The solution space is big, the exhaustive solution can't terminate
-- within reasonable time.
solve = dfs [[start]] [] where
  dfs [] s = s
  dfs (c:cs) s
    | head c == end = dfs cs (reverse c:s)
    | otherwise = dfs ((map (:c) $ moves c) ++ cs) s

moves (s:visited) = filter (`notElem` visited) [fwd s, bk s, cut s]
  where
    fwd xs = case break (0 ==) xs of
      (as, 0:b:bs) -> as ++ (b:0:bs)
      (a:as, [0]) -> 0:as ++ [a]
    bk xs = case break (0 ==) xs of
      ([], 0:bs) -> bs ++ [0]
      (as, 0:bs) -> (init as) ++ (0 : last as : bs)
    cut xs = case splitAt 4 xs of
      ((0:as), (x:bs)) -> (x:as) ++ (0:bs)
      ((x:as), (0:bs)) -> (0:as) ++ (x:bs)
      _ -> xs

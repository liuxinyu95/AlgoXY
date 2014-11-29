module ConwaySlide where

import Data.Map (insert, singleton, notMember, (!))

solve [] _ = [] -- no slution
solve (q@(n, _):qs) h | n == [0, 7, 6, 5, 4, 3, 2, 1] = backtrack q h []
                      | otherwise = let (cs, h') = (slide n h) in solve (qs ++ cs) h'

slide n h = ([(n', n) | n' <- ns], foldr (\n' h' -> insert n' n h') h ns) where
  ns = [n' | n' <- map ($ n) [left, right, up, down], n' /= [], n' `notMember` h]

-- xxxa0xxx -> xxx0axxx
right (0:xs) = (last xs):(init xs) ++ [0]
right xs = let (as, (_:bs)) = break (==0) xs in (init as) ++ (0 : last as : bs)

-- xxx0axxx -> xxxa0xxx
left xs = reverse $ right $ reverse xs

-- 0xxxaxxx -> axxx0xxx
down (0:a:b:c:d:xs) = d:a:b:c:0:xs
down _ = []

-- axxx0xxx -> 0xxxaxxx
up (a:b:c:d:0:xs) = 0:b:c:d:a:xs
up _ = []

backtrack (n, n') h ns | n == n' = ns
                       | otherwise = backtrack (n', h ! n') h (n:ns)

ans = solve [([0..7], [0..7])] (singleton [0..7] [0..7])

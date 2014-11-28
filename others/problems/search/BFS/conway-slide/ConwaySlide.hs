module ConwaySlide where

import Debug.Trace

solve [] _ = [] -- no slution
solve (q@(n:ns):qs) h | n == [0, 7, 6, 5, 4, 3, 2, 1] = reverse q
                      | otherwise = let (cs, h') = (slide q (trace ("q: " ++ show q) h)) in solve (qs ++ cs) h'

slide q@(n:_) h = (map (:q) ns, ns ++ h) where
  ns = [n' | n' <- map ($ n) [left, right, up, down], n' /= [], n' `notElem` h]

-- xxxa0xxx -> xxx0axxx
right (0:xs) = xs ++ [0]
right xs = let (as, (_:bs)) = break (==0) xs in (init as) ++ (0 : last as : bs)

-- xxx0axxx -> xxxa0xxx
left xs = reverse $ right $ reverse xs

-- 0xxxaxxx -> axxx0xxx
down (0:a:b:c:d:xs) = d:a:b:c:0:xs
down _ = []

-- axxx0xxx -> 0xxxaxxx
up (a:b:c:d:0:xs) = 0:b:c:d:a:xs
up _ = []

ans = solve [[[0 .. 7]]] []

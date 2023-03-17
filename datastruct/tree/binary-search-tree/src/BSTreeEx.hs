-- BST exercise

module BSTreeEx where

import BSTree hiding(min, max)
import Data.Function (on)
import Data.List (maximumBy)
import Test.QuickCheck

depth = foldt (const 1) (\x d y -> d + max x y) 0

maxDistance Empty = 0
maxDistance (Node Empty _ Empty) = 0
maxDistance (Node l _ r) = maximum [depth l + depth r, maxDistance l, maxDistance r]

maxPath Empty = []
maxPath (Node Empty k Empty) = [k]
maxPath (Node l k r) = longest [(reverse $ depthPath l) ++ k : depthPath r, maxPath l, maxPath r] where
  longest = maximumBy (compare `on` length)
  depthPath = foldt id (\ xs k ys -> k : longest [xs, ys]) []

maxDist = extract . mapTr where
  extract Empty = 0
  extract (Node _ (_, m) _) = m
  mapTr Empty = Empty
  mapTr (Node l _ r) = f (mapTr l) (mapTr r)
  f l r = Node l (1 + max d1 d2, maximum [d1 + d2, m1, m2]) r where
    (d1, m1) = pairof l
    (d2, m2) = pairof r
    pairof Empty = (0, 0)
    pairof (Node _ k _) = k

maxDist' = snd . pair . foldt id g Empty where
  g l _ r = Node l (1 + max d1 d2, maximum [d1 + d2, m1, m2]) r where
    (d1, m1) = pair l
    (d2, m2) = pair r
  pair = (maybe (0, 0) id) . key

prop_maxdistance :: (Ord a) => [a] -> Bool
prop_maxdistance xs = maxDistance t == max 0 (length (maxPath t) - 1) where
  t = fromList xs

prop_maxdist :: (Ord a) => [a] -> Bool
prop_maxdist xs = maxDistance t == maxDist t where t = fromList xs

prop_maxdist' :: (Ord a) => [a] -> Bool
prop_maxdist' xs = maxDistance t == maxDist' t where t = fromList xs

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
maxPath (Node l k r) = longest [(reverse depthPath l) ++ k : depthPath r, maxPath l, maxPath r] where
  longest = maximumBy (compare `on` length)
  depthPath = foldt id (\xs k ys -> k : longest [xs, ys]) []

prop_maxdist :: (Ord a) => [a] -> Bool
prop_maxdist xs = maxDistance t == max 0 (length (maxPath t) - 1) where
  t = fromList xs

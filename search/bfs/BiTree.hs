module BiTree where

import qualified Data.Sequence as Q
import Data.Sequence (Seq((:<|)), (|>))
import Test.QuickCheck
import Data.List (nub, sort, group, isPrefixOf)

data Tr a = E | Br (Tr a) a (Tr a)

-- traverse the binary tree level by level (BFS order).
toList t = bfs (Q.singleton t) where
  bfs Q.Empty = []
  bfs (E :<| q) = bfs q
  bfs ((Br l k r) :<| q) = k : bfs (q |> l |> r)

-- verification

-- Augment the binary search tree node with depth
fromList = foldr (ins 0) E where
  ins d x E = Br E (x, d) E
  ins d x (Br l (k, d') r)
    | x < k = Br (ins (d + 1) x l) (k, d') r
    | x > k = Br l (k, d') (ins (d + 1) x r)

prop_leveled :: [Int] -> Bool
prop_leveled xs = leveled && (sort zs == sort ys) where
  ys = nub xs
  (zs, ds) = unzip $ toList $ fromList ys
  leveled = map head (group ds) `isPrefixOf` [0..]

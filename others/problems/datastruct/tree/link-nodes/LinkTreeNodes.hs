module LinkTreeNodes where

data Tree a = Empty | Branch (Tree a) a (Tree a)
              deriving (Eq, Show)

left (Branch l _ _) = l
right (Branch _ _ r) = r
key (Branch _ k _) = k

link t = link' [[t]] [[key t]] where
      link' [] acc = reverse acc
      link' (ts:tss) acc = let ts' = concat [filter (/=Empty) [left tr, right tr] | tr <- ts] in
          if ts' == [] then link' tss acc else link' (tss ++ [ts']) ((map key ts'):acc)

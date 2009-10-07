-- Int (as key) Patricia Tree
-- Referred from Haskell packages Data.IntMap
-- Other reference includes:
--  [1] CLRS, Problems 12-2: Radix trees
--  [2] Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--	Workshop on ML, September 1998, pages 77-86,
--	<http://www.cse.ogi.edu/~andy/pub/finite.htm>

import Data.Bits

-- 1. Little Edian Patricia tree
data IntTree a = Empty 
               | Leaf Key a
               | Branch Prefix Mask (IntTree a) (IntTree a) -- prefix, mask, left, right

type Key = Int
type Prefix = Int
type Mask = Int

-- helpers
-- join 2 nodes together.
-- (prefix1, tree1) ++ (prefix2, tree2)
--  1. find the longest common prefix == common(prefix1, prefix2)
--         prefix1 = a(n),a(n-1),...a(i+1),a(i),x...
--         prefix2 = a(n),a(n-1),...a(i+1),a(i),y...
--         prefix  = a(n),a(n-1),...a(i+1),a(i),00...0
--  2. mask bit = 100...0b (=2^i)
--         so mask is something like, 1,2,4,...,128,256,...
--  3. if      x=='0', y=='1' then (tree1=>left, tree2=>right), 
--     else if x=='1', y=='0' then (tree2=>left, tree1=>right).
join :: Prefix -> IntTree a -> Prefix -> IntTree a -> IntTree a
join p1 t1 p2 t2 = if zeroBit p1 m then Branch p m t1 t2
                                   else Branch p m t2 t1 
    where
      (prefix, mask) = lcf p1 p2

-- 'lcf' means 'longest common prefix'
lcf :: Prefix -> Prefix -> (Prefix, Mask)
lcf p1 p2 = highestBit (p1 `xor` p2)

highestBit :: Int -> Int
highestBit x = if x==0 then 0 else 1+highestBit (shiftR x 1)

-- Insertion
-- if user insert a value already binding with existed key, 
-- just over write the previous value
-- usage: insert tree key x
insert :: IntTree a -> Key -> a -> IntTree a
insert t k x 
   = case t of
       Empty -> Leaf k x
       Leaf k' x' -> if k==k' then Leaf k x
                     else join k (Leaf k x) k' t -- t@(Leaf k' x')
       Branch p m l r
          | matchPrefix k p m -> if zeroBit k m
                                 then Branch p m (insert l k x) r
                                 else Branch p m l (insert r k x)
          | otherwise join k (Leaf k x) p t

-- Look up
search :: IntTree a -> Key -> Maybe a
search Empty k = Nothing
search t 0 = value t
search t k = if even k then search (left t) (k `div` 2)
             else search (right t) (k `div` 2)

-- Test helper
fromList :: [(Key, a)] -> IntTree a
fromList xs = foldl ins Empty xs where
    ins t (k, v) = insert t k v

-- k = ... a2, a1, a0 ==> k' = ai * m + k, where m=2^i
toString :: (Show a)=>IntTree a -> String
toString t = toStr t 0 1 where
    toStr Empty k m = "."
    toStr tr k m = "(" ++ (toStr (left tr) k (2*m)) ++
                      " " ++ (show k) ++ (valueStr (value tr)) ++
                      " " ++ (toStr (right tr) (m+k) (2*m)) ++ ")"
    valueStr (Just x) = ":" ++ (show x)
    valueStr _ = ""

-- Test cases
testIntTree = "t=" ++ (toString t) ++ "\nsearch t 4: " ++ (show $ search t 4) ++
              "\nsearch t 0: " ++ (show $ search t 0)
    where
      t = fromList [(1, 'x'), (4, 'y'), (5, 'z')]

main = do
  putStrLn testIntTree

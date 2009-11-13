type Key = String

data Patricia a = Patricia { value :: Maybe a
                           , children :: [(Key, Patricia a)]}

empty = Patrcia Nothing []

leaf :: a -> Patricia a
leaf x = Patricia (Just x) []

--insert
insert :: Patricia a -> Key -> a -> Patricia a
insert t k x = Patricia (value t) ins (children t) k x where
    ins []     k x = [(k, Patricia (Just x) [])]
    ins (p:ps) k x 
        | (fst p) == k 
            = (k, Patricia (Just x) children (snd p)):ps --overwrite
        | match (fst p) k 
            = (branch k (leaf x) (fst p) (snd p)):ps
        | otherwise 
            = p:(ins ps k x)

match :: Key -> Key -> Bool
match [] _ = False
match _ [] = False
match x y = head x == head y

branch :: Key -> Patricia a -> Key -> Patricia a -> (Key, Patricia a)
branch k1 t1 k2 t2 
    | k1'=="" = (k, Patricia (value t1) [(k2', t2)])
    | k2'=="" = (k, Patricia (value t2) [(k1', t1)])
    | otherwise = (k, Patricia Nothing [(k1', t1), (k2', t2)]) 
   where
      k = lcp k1 k2
      k1' = drop (length k) k1
      k2' = drop (length k) k2

lcp :: Key -> Key -> Key
lcp [] _ = []
lcp _ [] = []
lcp (x:xs) (y:ys) = if x==y then x:(lcp xs ys) else []
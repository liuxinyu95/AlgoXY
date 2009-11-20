-- Refer to http://en.wikipedia.org/wiki/Trie

-- definition
data Trie a = Trie { value :: Maybe a
                   , children :: [(Char, Trie a)]}

empty = Trie Nothing []

-- insert
-- usage: insert t "zoo" "a place where animals are for public to see"
insert :: Trie a -> String -> a -> Trie a
insert t []     x = Trie (Just x)  (children t)
insert t (k:ks) x = Trie (value t) (ins (children t) k ks x) where
    ins [] k ks x = [(k, (insert empty ks x))]
    ins (p:ps) k ks x = if fst p == k 
                        then (k, insert (snd p) ks x):ps
                        else p:(ins ps k ks x)

-- lookup
find :: Trie a -> String -> Maybe a
find t [] = value t
find t (k:ks) = case lookup k (children t) of
                  Nothing -> Nothing
                  Just t' -> find t' ks

fromList :: [(String, a)] -> Trie a
fromList xs = foldl ins empty xs where
    ins t (k, v) = insert t k v

sort :: (Ord a)=>[(a, b)] -> [(a, b)]
sort [] = []
sort (p:ps) = sort xs ++ [p] ++ sort ys where
    xs = [x | x<-ps, fst x <= fst p ]
    ys = [y | y<-ps, fst y > fst p ]

toString :: (Show a)=> Trie a -> String
toString t = toStr t "" where
    toStr t prefix = "(" ++ prefix ++ showMaybe (value t) ++ 
                     (concat $ map (\(k, v)-> ", " ++ toStr v (prefix++[k])) 
                                 (sort $ children t))
                     ++ ")"
    showMaybe Nothing = ""
    showMaybe (Just x)  = ":" ++ show x

testTrie = "t=" ++ (toString t) ++ 
           "\nt'=" ++ (toString t') ++
           "\nsearch t an: " ++ (show $ find t "an") ++
           "\nsearch t boy: " ++ (show $ find t "boy") ++
           "\nsearch t the: " ++ (show $ find t "the")
    where 
      t = fromList [("a", 1), ("an", 2), ("another", 7), ("boy", 3), ("bool", 4), ("zoo", 3)]
      t'= fromList [("zoo", 3), ("bool", 4), ("boy", 3), ("another", 7), ("an", 2), ("a", 1)]

main = do
  putStrLn testTrie
